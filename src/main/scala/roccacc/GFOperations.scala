package roccacc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile.CoreModule

/** Galois Field Operations Module
  * 
  * This module provides basic Galois Field arithmetic operations including:
  * - Addition (XOR in GF)
  * - Multiplication 
  * - Division
  * - Exponentiation
  * - Inverse computation
  * 
  * Currently supports GF(2^m) fields where m is configurable
  */
object GFOperations {
  /** Default field size for GF(2^8) */
  val DEFAULT_FIELD_SIZE = 8
  
  /** Function codes for different GF operations */
  val SZ_GF_FN = 4.W
  
  def FN_X = BitPat("b????")
  def FN_ADD = BitPat("b0000")  // GF Addition (XOR)
  def FN_MUL = BitPat("b0001")  // GF Multiplication
  def FN_DIV = BitPat("b0010")  // GF Division
  def FN_POW = BitPat("b0011")  // GF Exponentiation
  def FN_INV = BitPat("b0100")  // GF Inverse
  def FN_RED = BitPat("b0101")  // GF Reduction
  def FN_GIP = BitPat("b0110")  // GF Irreducible Polynomial
  
  // Helper functions to check operation type
  def isAdd(cmd: UInt) = cmd === FN_ADD
  def isMul(cmd: UInt) = cmd === FN_MUL
  def isDiv(cmd: UInt) = cmd === FN_DIV
  def isPow(cmd: UInt) = cmd === FN_POW
  def isInv(cmd: UInt) = cmd === FN_INV
  def isRed(cmd: UInt) = cmd === FN_RED
  def isGip(cmd: UInt) = cmd === FN_GIP
}

class GFReduce(fieldSize: Int = GFOperations.DEFAULT_FIELD_SIZE) extends Module {
  val io = IO(new Bundle {
    val in1 = Flipped(Decoupled(UInt((2 * fieldSize).W)))
    val out = Valid(UInt(fieldSize.W))
  })

  // Build the Mastrovito reduction matrix once at elaboration; wire up combinational XOR network
  private val reductionMatrix = GFReduceGen.buildReductionMatrix(fieldSize, GFReduceGen.GF256_POLY)

  val outBits = Wire(Vec(fieldSize, Bool()))
  // unroll the reduction matrix
  for (j <- 0 until fieldSize) {
    outBits(j) := reductionMatrix(j).map(i => io.in1.bits(i)).reduce(_ ^ _)
  }

  io.in1.ready := true.B
  io.out.valid := io.in1.valid
  io.out.bits   := outBits.asUInt
}

class GFAdd(fieldSize: Int = GFOperations.DEFAULT_FIELD_SIZE) extends Module {
  val io = IO(new Bundle {
    val in1 = Flipped(Decoupled(UInt((2 * fieldSize).W)))
    val in2 = Flipped(Decoupled(UInt((2 * fieldSize).W)))
    val out = Valid(UInt(fieldSize.W))
  })

  // Both inputs are reduced combinationally; the XOR is latched on the next clock edge.
  // This gives a 1-cycle latency that matches the Send/Wait (sAddS/sAddW) caller pattern.
  val reducer1 = Module(new GFReduce(fieldSize))
  val reducer2 = Module(new GFReduce(fieldSize))

  reducer1.io.in1.bits  := io.in1.bits
  reducer1.io.in1.valid := io.in1.valid
  reducer2.io.in1.bits  := io.in2.bits
  reducer2.io.in1.valid := io.in2.valid

  val result_reg = RegInit(0.U(fieldSize.W))
  val valid_reg  = RegInit(false.B)

  when(io.in1.valid && io.in2.valid) {
    result_reg := reducer1.io.out.bits ^ reducer2.io.out.bits
    valid_reg  := true.B
  }.otherwise {
    valid_reg := false.B
  }

  io.in1.ready := true.B
  io.in2.ready := true.B
  io.out.valid := valid_reg
  io.out.bits  := result_reg
}

class GFMul(fieldSize: Int = GFOperations.DEFAULT_FIELD_SIZE) extends Module {
    val io = IO(new Bundle {
    val in1 = Flipped(Decoupled(UInt((2 * fieldSize).W)))
    val in2 = Flipped(Decoupled(UInt((2 * fieldSize).W)))
    val out = Valid(UInt(fieldSize.W))
  })
  object MulState extends ChiselEnum {
    val idle, reduction, accumulation, r_reduction, done = Value
  }
  val mul_state = RegInit(MulState.idle)
  val mul_result = RegInit(0.U((fieldSize).W))
  val reducer1_done = RegInit(false.B)
  val reducer2_done = RegInit(false.B)
  val reducer1_sent = RegInit(false.B)
  val reducer2_sent = RegInit(false.B)
  val reducer3_sent = RegInit(false.B)
  val reduced1 = RegInit(0.U((2 * fieldSize).W))
  val reduced2 = RegInit(0.U((2 * fieldSize).W))
  val storedinput1 = RegInit(0.U((2 * fieldSize).W))
  val storedinput2 = RegInit(0.U((2 * fieldSize).W))
  io.in1.ready := mul_state === MulState.idle
  io.in2.ready := mul_state === MulState.idle
  io.out.bits := 0.U((fieldSize).W)
  io.out.valid := false.B
  val reducer1 = Module(new GFReduce(fieldSize))
  val reducer2 = Module(new GFReduce(fieldSize))
  val reducer3 = Module(new GFReduce(fieldSize))
  reducer1.io.in1.bits := 0.U((2 * fieldSize).W)
  reducer1.io.in1.valid := false.B
  reducer2.io.in1.bits := 0.U((2 * fieldSize).W)
  reducer2.io.in1.valid := false.B
  reducer3.io.in1.bits := 0.U((2 * fieldSize).W)
  reducer3.io.in1.valid := false.B
  val acccount = RegInit(0.U((log2Ceil(fieldSize)+1).W))
  //Should do log2(fieldSize) instead of fieldSize
  val accval = RegInit(0.U((2 * fieldSize).W))

  switch(mul_state) {
  is(MulState.idle) { // IDLE
    printf("(Multiplication) in idle state, waiting for inputs\n")
    reducer1_sent := false.B
    reducer2_sent := false.B
    reducer1_done := false.B
    reducer2_done := false.B
    reducer3_sent := false.B
    mul_result := 0.U((fieldSize).W)
    io.out.valid := false.B
    io.out.bits := 0.U((fieldSize).W)
    acccount := 0.U((log2Ceil(fieldSize)+1).W)
    when(io.in1.valid && io.in2.valid) {
      storedinput1 := io.in1.bits
      storedinput2 := io.in2.bits
      mul_state := MulState.reduction
    }
  }
is(MulState.reduction) { // REDUCTION
    printf("(Multiplication) in reduction state\n")
    when(!reducer1_sent) {
  reducer1.io.in1.bits := storedinput1
    reducer1.io.in1.valid := true.B
    reducer1_sent := true.B
  }
  when(!reducer2_sent) {
    reducer2.io.in1.bits := storedinput2
    reducer2.io.in1.valid := true.B
    reducer2_sent := true.B
  }
  when(reducer1.io.out.valid) {
    printf("(Multiplication)reducer1 done, returning %b\n", reducer1.io.out.bits)
    reduced1 := reducer1.io.out.bits
    reducer1_done := true.B
  }
  when(reducer2.io.out.valid) {
    printf("(Multiplication)reducer2 done, returning %b\n", reducer2.io.out.bits)
    reduced2 := reducer2.io.out.bits
    reducer2_done := true.B
  }
    when(reducer1_done && reducer2_done) {
        printf("(Multiplication) reducers done, multiplying %b and %b\n", reduced1, reduced2)
        reducer1.io.in1.valid := false.B
        reducer2.io.in1.valid := false.B
        mul_state := MulState.accumulation
    }
}
is(MulState.accumulation) {
  printf("(Multiplication) in accumulation state\n")
  when(acccount < fieldSize.U){
    printf("(Multiplication) acccount %d, accval %b\n", acccount, accval)
      accval := Mux(reduced2(acccount), accval ^ reduced1 << acccount, accval)
      acccount := acccount + 1.U
  }.otherwise{
    mul_state := MulState.r_reduction
  }
}
is(MulState.r_reduction){
   when(!reducer3_sent) {
    reducer3.io.in1.bits := accval
    reducer3.io.in1.valid := true.B
    reducer3_sent := true.B
  }
  when(reducer3.io.out.valid) {
    printf("(Multiplication)reducer3 done, returning %b\n", reducer3.io.out.bits)
    mul_result := reducer3.io.out.bits
    mul_state := MulState.done
  }
}
is(MulState.done) { // DONE
    printf("(Multiplication) done, returning %b\n", mul_result)
    reducer1.io.in1.bits := 0.U((2 * fieldSize).W)
    reducer2.io.in1.bits := 0.U((2 * fieldSize).W)
    io.out.valid := true.B
    io.out.bits := mul_result
    mul_state := MulState.idle
    reduced1 := 0.U((2 * fieldSize).W)
    reduced2 := 0.U((2 * fieldSize).W)
    accval := 0.U((2 * fieldSize).W)
    storedinput1 := 0.U((2 * fieldSize).W)
    storedinput2 := 0.U((2 * fieldSize).W)
    }
  }
}

class GFPower(fieldSize: Int = GFOperations.DEFAULT_FIELD_SIZE) extends Module {
    val io = IO(new Bundle {
    val in1 = Flipped(Decoupled(UInt((2 * fieldSize).W)))
    val in2 = Flipped(Decoupled(UInt((2 * fieldSize).W)))
    val out = Valid(UInt(fieldSize.W))
  })
  object PowerState extends ChiselEnum {
    val idle, computing, done = Value
  }
  val power_state = RegInit(PowerState.idle)
  val power_result = RegInit(0.U((fieldSize).W))
  val load_inputs_done = RegInit(false.B)
  val acc_val = RegInit(1.U((fieldSize).W))
  val acc_count = RegInit(0.U((2 * fieldSize).W))
  val storedinput1 = RegInit(0.U((2 * fieldSize).W))
  val storedinput2 = RegInit(0.U((2 * fieldSize).W))
  io.in1.ready := power_state === PowerState.idle
  io.in2.ready := power_state === PowerState.idle
  io.out.bits := 0.U((fieldSize).W)
  io.out.valid := false.B
  val multiplier1 = Module(new GFMul(fieldSize))
  multiplier1.io.in1.bits := 0.U((2 * fieldSize).W)
  multiplier1.io.in1.valid := false.B
  multiplier1.io.in2.bits := 0.U((2 * fieldSize).W)
  multiplier1.io.in2.valid := false.B

  switch(power_state) {
  is(PowerState.idle) { // IDLE
    printf("(Power) in idle state, waiting for inputs\n")
    load_inputs_done := false.B
    power_result := 0.U((fieldSize).W)
    io.out.valid := false.B
    io.out.bits := 0.U((fieldSize).W)
    when(io.in1.valid && io.in2.valid) {
      power_state := PowerState.computing
      storedinput1 := io.in1.bits
      storedinput2 := io.in2.bits
    }
  }
  is(PowerState.computing) { // COMPUTING
    printf("(Power) in computing state, %b to the power of %b\n", storedinput1, storedinput2)
    printf("(Power) acc_val %b, acc_count %d\n", acc_val, acc_count)
    when(!load_inputs_done && multiplier1.io.in1.ready && multiplier1.io.in2.ready) {
      printf("(Power) loading inputs\n")
      multiplier1.io.in1.bits := storedinput1
      multiplier1.io.in1.valid := true.B
      multiplier1.io.in2.bits := storedinput1
      multiplier1.io.in2.valid := true.B
      load_inputs_done := true.B
      acc_val := 1.U((fieldSize).W)
      acc_count := 2.U((2 * fieldSize).W)
    }
    .elsewhen(load_inputs_done && multiplier1.io.in1.ready && multiplier1.io.in2.ready){
      when(multiplier1.io.in1.ready && multiplier1.io.in2.ready) {
        multiplier1.io.in1.bits := storedinput1
        multiplier1.io.in1.valid := true.B
        multiplier1.io.in2.bits := acc_val
        multiplier1.io.in2.valid := true.B
      }
    }
    when(acc_count < storedinput2 && multiplier1.io.out.valid){
      acc_val := multiplier1.io.out.bits
      acc_count := acc_count + 1.U
    }
    .elsewhen(multiplier1.io.out.valid){
      printf("(Power) multiplier done, returning %b\n", multiplier1.io.out.bits)
      acc_val := multiplier1.io.out.bits
      power_state := PowerState.done
      multiplier1.io.in1.valid := false.B
      multiplier1.io.in2.valid := false.B
    }
  }
  is(PowerState.done) { // DONE
      printf("(Power) done, returning %b\n", acc_val)
      io.out.valid := true.B
      io.out.bits := acc_val
      power_state := PowerState.idle
      acc_val := 0.U((fieldSize).W)
      acc_count := 0.U((2 * fieldSize).W)
      load_inputs_done := false.B
      storedinput1 := 0.U((2 * fieldSize).W)
      storedinput2 := 0.U((2 * fieldSize).W)
    }
  }
}

class GFDiv(fieldSize: Int = GFOperations.DEFAULT_FIELD_SIZE) extends Module {
    val io = IO(new Bundle {
    val in1 = Flipped(Decoupled(UInt((2 * fieldSize).W)))
    val in2 = Flipped(Decoupled(UInt((2 * fieldSize).W)))
    val out = Valid(UInt(fieldSize.W))
  })
  object DivState extends ChiselEnum {
    val idle, exponentiation, multiplication, done = Value
  }
  val div_state = RegInit(DivState.idle)
  val inv_result = RegInit(0.U((fieldSize).W))
  val div_result = RegInit(0.U((fieldSize).W))
  io.in1.ready := div_state === DivState.idle
  io.in2.ready := div_state === DivState.idle
  io.out.bits := 0.U((fieldSize).W)
  io.out.valid := false.B
  val multiplier1 = Module(new GFMul(fieldSize))
  val exponentiator1 = Module(new GFPower(fieldSize))
  val storedinput1 = RegInit(0.U((2 * fieldSize).W))
  val storedinput2 = RegInit(0.U((2 * fieldSize).W))
  multiplier1.io.in1.bits := 0.U((2 * fieldSize).W)
  multiplier1.io.in1.valid := false.B
  multiplier1.io.in2.bits := 0.U((2 * fieldSize).W)
  multiplier1.io.in2.valid := false.B
  exponentiator1.io.in1.bits := 0.U((2 * fieldSize).W)
  exponentiator1.io.in1.valid := false.B
  exponentiator1.io.in2.bits := 0.U((2 * fieldSize).W)
  exponentiator1.io.in2.valid := false.B

  switch(div_state) {
  is(DivState.idle) { // IDLE
    printf("(Div) in idle state, waiting for inputs\n")
    div_result := 0.U((fieldSize).W)
    io.out.valid := false.B
    io.out.bits := 0.U((fieldSize).W)
    when(io.in1.valid && io.in2.valid) {
      div_state := DivState.exponentiation
      storedinput1 := io.in1.bits
      storedinput2 := io.in2.bits
    }
  }
  is(DivState.exponentiation) { // EXPONENTIATION
    printf("(Div) in exponentiation state, dividing %b by %b\n", storedinput1, storedinput2)
    when(exponentiator1.io.in1.ready && exponentiator1.io.in2.ready) {
      exponentiator1.io.in2.bits := ((BigInt(1) << fieldSize) - 2).U((2 * fieldSize).W)
      exponentiator1.io.in2.valid := true.B
      exponentiator1.io.in1.bits := storedinput2
      exponentiator1.io.in1.valid := true.B
    }
    when(exponentiator1.io.out.valid) {
      printf("(Div) exponentiation done, returning %b\n", exponentiator1.io.out.bits)
      inv_result := exponentiator1.io.out.bits
      exponentiator1.io.in1.valid := false.B
      exponentiator1.io.in2.valid := false.B
      div_state := DivState.multiplication
    }
  }
  is(DivState.multiplication) { // MULTIPLICATION
    printf("(Div) in multiplication state\n")
    when(multiplier1.io.in1.ready && multiplier1.io.in2.ready) {
      multiplier1.io.in1.bits := inv_result
      multiplier1.io.in1.valid := true.B
      multiplier1.io.in2.bits := storedinput1
      multiplier1.io.in2.valid := true.B
    }
    when(multiplier1.io.out.valid) {
      printf("(Div) multiplication done, returning %b\n", multiplier1.io.out.bits)
      div_result := multiplier1.io.out.bits
      multiplier1.io.in1.valid := false.B
      multiplier1.io.in2.valid := false.B
      div_state := DivState.done
    }
  }
  is(DivState.done) { // DONE
      printf("(Div) done, returning %b\n", div_result)
      io.out.valid := true.B
      io.out.bits := div_result
      div_state := DivState.idle
      inv_result := 0.U((fieldSize).W)
      div_result := 0.U((fieldSize).W)
      storedinput1 := 0.U((2 * fieldSize).W)
      storedinput2 := 0.U((2 * fieldSize).W)
    }
  }
}
//   /** Get irreducible polynomial for given field size
//     * 
//     * @param fieldSize Size of the field
//     * @return Irreducible polynomial
//     */
//   def getIrreduciblePolynomial(fieldSize: Int): UInt = {
//     fieldSize match {
//       case 8 => "b100011101".U  // x^8 + x^4 + x^3 + x^2 + 1
//       case 16 => "b10000000000011011".U  // x^16 + x^5 + x^3 + x + 1
//       case 32 => "b100000000000000000000000000101101".U  // x^32 + x^7 + x^3 + x^2 + 1
//       case _ => "b100011101".U  // Default to GF(2^8) polynomial
//     }
//   }
// }

