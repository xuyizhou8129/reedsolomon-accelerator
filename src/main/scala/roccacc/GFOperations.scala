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
  object ReducerState extends ChiselEnum {
    val idle, computing, done = Value
  }

  val reduce_state = RegInit(ReducerState.idle)
  val reduce_result = RegInit(0.U((fieldSize).W))
  val reduce_temp_poly = RegInit(0.U((2 * fieldSize).W))
  val reduce_irreducible = "b100011101".U
  io.in1.ready := reduce_state === ReducerState.idle
  io.out.bits := 8.U((fieldSize).W)
  io.out.valid := false.B
  
  switch(reduce_state) {
  is(ReducerState.idle) { // IDLE
    io.out.valid := false.B
    io.out.bits := 8.U((fieldSize).W)
    when(io.in1.valid) {
      reduce_state := ReducerState.computing
      reduce_temp_poly := io.in1.bits
      printf("(Reduction) reduce called, Reducing %b with irreducible %x\n", io.in1.bits, reduce_irreducible)
    }
  }
  
is(ReducerState.computing) { // COMPUTING
    printf("(Reduction) in computing state, reducing %b\n", reduce_temp_poly)
    when(reduce_temp_poly === 0.U) {
        reduce_result := 0.U
        printf("(Reduction) reduced to 0, returning 0\n")
        reduce_state := ReducerState.done
    }.otherwise {
        // Find the position of the highest set bit
        val msb = PriorityEncoder(Reverse(reduce_temp_poly))
        val polyWidth = (fieldSize * 2).U
        val actualMsb = polyWidth - 1.U - msb  // Convert to actual MSB position
        
        when(actualMsb < fieldSize.U) {
            // Already reduced
            reduce_result := reduce_temp_poly(fieldSize - 1, 0)
            reduce_state := ReducerState.done
        }.otherwise {
            // Need to reduce
            val shiftAmount = actualMsb - fieldSize.U
            reduce_temp_poly := reduce_temp_poly ^ (reduce_irreducible << shiftAmount)
            // Stay in COMPUTING
        }
    }
}
is(ReducerState.done) { // DONE
    printf("(Reduction) done, returning %b\n", reduce_result)
    reduce_state := ReducerState.idle
    io.out.valid := true.B
    io.out.bits := reduce_result
    }
  }
}

class GFAdd(fieldSize: Int = GFOperations.DEFAULT_FIELD_SIZE) extends Module {
    val io = IO(new Bundle {
    val in1 = Flipped(Decoupled(UInt((2 * fieldSize).W)))
    val in2 = Flipped(Decoupled(UInt((2 * fieldSize).W)))
    val out = Valid(UInt(fieldSize.W))
  })
  object AdderState extends ChiselEnum {
    val idle, computing, done = Value
  }
  val adder_state = RegInit(AdderState.idle)
  val adder_result = RegInit(0.U((fieldSize).W))
  val reducer1_done = RegInit(false.B)
  val reducer2_done = RegInit(false.B)
  val reducer1_sent = RegInit(false.B)
  val reducer2_sent = RegInit(false.B)
  val reduced1 = RegInit(0.U((2 * fieldSize).W))
  val reduced2 = RegInit(0.U((2 * fieldSize).W))
  io.in1.ready := adder_state === AdderState.idle
  io.in2.ready := adder_state === AdderState.idle
  io.out.bits := 0.U((fieldSize).W)
  io.out.valid := false.B
  val reducer1 = Module(new GFReduce(fieldSize))
  val reducer2 = Module(new GFReduce(fieldSize))
  reducer1.io.in1.bits := 0.U((2 * fieldSize).W)
  reducer1.io.in1.valid := false.B
  reducer2.io.in1.bits := 0.U((2 * fieldSize).W)
  reducer2.io.in1.valid := false.B

  switch(adder_state) {
  is(AdderState.idle) { // IDLE
    printf("(Addition) in idle state, waiting for inputs\n")
    reducer1_sent := false.B
    reducer2_sent := false.B
    reducer1_done := false.B
    reducer2_done := false.B
    adder_result := 0.U((fieldSize).W)
    io.out.valid := false.B
    io.out.bits := 0.U((fieldSize).W)
    when(io.in1.valid && io.in2.valid) {
      adder_state := AdderState.computing
    }
  }
is(AdderState.computing) { // COMPUTING
    printf("(Addition) in computing state, reducing inputs\n")
    when(!reducer1_sent) {
  reducer1.io.in1.bits := io.in1.bits
    reducer1.io.in1.valid := true.B
    reducer1_sent := true.B
  }
  when(!reducer2_sent) {
    reducer2.io.in1.bits := io.in2.bits
    reducer2.io.in1.valid := true.B
    reducer2_sent := true.B
  }
  when(reducer1.io.out.valid) {
    printf("(Addition)reducer1 done, returning %b\n", reducer1.io.out.bits)
    reduced1 := reducer1.io.out.bits
    reducer1_done := true.B
    reducer1.io.in1.valid := false.B
  }
  when(reducer2.io.out.valid) {
    printf("(Addition)reducer2 done, returning %b\n", reducer2.io.out.bits)
    reduced2 := reducer2.io.out.bits
    reducer2_done := true.B
    reducer2.io.in1.valid := false.B
  }
    when(reducer1_done && reducer2_done) {
        printf("(Addition) reducers done, adding %b and %b\n", reduced1, reduced2)
        adder_result := reduced1 ^ reduced2
        reducer1.io.in1.valid := false.B
        reducer2.io.in1.valid := false.B
        adder_state := AdderState.done
    }
}
is(AdderState.done) { // DONE
    printf("(Addition) done, returning %b\n", adder_result)
    reducer1.io.in1.bits := 0.U((2 * fieldSize).W)
    reducer2.io.in1.bits := 0.U((2 * fieldSize).W)
    io.out.valid := true.B
    io.out.bits := adder_result
    adder_state := AdderState.idle
    reduced1 := 0.U((2 * fieldSize).W)
    reduced2 := 0.U((2 * fieldSize).W)
    }
  }
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
      mul_state := MulState.reduction
    }
  }
is(MulState.reduction) { // REDUCTION
    printf("(Multiplication) in reduction state\n")
    when(!reducer1_sent) {
  reducer1.io.in1.bits := io.in1.bits
    reducer1.io.in1.valid := true.B
    reducer1_sent := true.B
  }
  when(!reducer2_sent) {
    reducer2.io.in1.bits := io.in2.bits
    reducer2.io.in1.valid := true.B
    reducer2_sent := true.B
  }
  when(reducer1.io.out.valid) {
    printf("(Multiplication)reducer1 done, returning %b\n", reducer1.io.out.bits)
    reduced1 := reducer1.io.out.bits
    reducer1_done := true.B
    reducer1.io.in1.valid := false.B
  }
  when(reducer2.io.out.valid) {
    printf("(Multiplication)reducer2 done, returning %b\n", reducer2.io.out.bits)
    reduced2 := reducer2.io.out.bits
    reducer2_done := true.B
    reducer2.io.in1.valid := false.B
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
    reducer3.io.in1.valid := false.B
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
    }
  }
}

/** Implementation of Galois Field Operations
  * 
  * @param fieldSize The size of the Galois Field (e.g., 8 for GF(2^8))
  * @param p Implicit parameter passed by the build system
  */
class GFOperations(fieldSize: Int = GFOperations.DEFAULT_FIELD_SIZE)(implicit p: Parameters) extends CoreModule()(p) {
  import GFOperations._
  
  val io = IO(new Bundle {
    val fn = Input(Bits(SZ_GF_FN))           // Operation function code
    val operand1 = Input(UInt((2 * fieldSize).W))  // First operand
    val operand2 = Input(UInt((2 * fieldSize).W))  // Second operand (not used for inverse)
    val result = Output(UInt((2 * fieldSize).W))  // Operation result
    val valid = Output(Bool())               // Result valid signal
  })
  
  // Internal registers for state management
  val result_valid = RegInit(false.B)
  // Default outputs
  io.result := 0.U(fieldSize.W)
  io.valid := result_valid
  
val reduce_state = RegInit(0.U(2.W))  // 0: IDLE, 1: COMPUTING, 2: DONE
val reduce_temp_poly = RegInit(0.U((fieldSize * 2).W))
val reduce_irreducible = RegInit(0.U((fieldSize + 1).W))
val reduce_result = RegInit(0.U(fieldSize.W))
val reduce_requested = RegInit(false.B)

  // GF Addition (XOR operation)
  //*Todo: Implement the Valid Singal Properly
  when(isAdd(io.fn)) {
    io.result := gfAdd(io.operand1, io.operand2, fieldSize)
    result_valid := true.B
  }.elsewhen(isMul(io.fn)) {
    // GF Multiplication using polynomial multiplication
    io.result := gfMultiply(io.operand1, io.operand2, fieldSize)
    result_valid := true.B
  }.elsewhen(isDiv(io.fn)) {
    // GF Division (multiply by inverse)
    val inverse = gfInverse(io.operand2, fieldSize)
    io.result := gfMultiply(io.operand1, inverse, fieldSize)
    result_valid := true.B
  }.elsewhen(isPow(io.fn)) {
    // GF Exponentiation
    io.result := gfPower(io.operand1, io.operand2, fieldSize)
    result_valid := true.B
  }.elsewhen(isInv(io.fn)) {
    // GF Inverse
    io.result := gfInverse(io.operand1, fieldSize)
    result_valid := true.B
  }.elsewhen(isRed(io.fn)) {
    // GF Reduction
    io.result := gfReduce(io.operand1, io.operand2, fieldSize)
    result_valid := true.B
  }.elsewhen(isGip(io.fn)) {
    // GF Irreducible Polynomial
    io.result := getIrreduciblePolynomial(fieldSize)
    result_valid := true.B
  }.otherwise {
    // No valid operation - clear valid signal
    result_valid := false.B
  }
  
  /** Galois Field multiplication using polynomial representation
    * 
    * @param a First operand
    * @param b Second operand  
    * @param fieldSize Size of the field
    * @return Result of GF multiplication
    */
  def gfAdd(a: UInt, b: UInt, fieldSize: Int): UInt = {
    val result = Wire(UInt(fieldSize.W))
    // Reduce inputs first, then do GF addition (XOR)
    val reducedA = gfReduce(a, getIrreduciblePolynomial(fieldSize), fieldSize)
    val reducedB = gfReduce(b, getIrreduciblePolynomial(fieldSize), fieldSize)
    result := reducedA ^ reducedB
    result
  }

  def gfMultiply(a: UInt, b: UInt, fieldSize: Int): UInt = {
    val result = Wire(UInt(fieldSize.W))
    
    // Handle zero case
    when(a === 0.U || b === 0.U) {
      result := 0.U
    }.otherwise {
      // Proper GF multiplication using unrolled polynomial multiplication
      // Manually unroll the loop to avoid combinational cycles
      //* Todo: Implement this properly without hard coding the number of terms
      val term0 = Mux(b(0), a << 0.U, 0.U)
      val term1 = Mux(b(1), a << 1.U, 0.U)
      val term2 = Mux(b(2), a << 2.U, 0.U)
      val term3 = Mux(b(3), a << 3.U, 0.U)
      val term4 = Mux(b(4), a << 4.U, 0.U)
      val term5 = Mux(b(5), a << 5.U, 0.U)
      val term6 = Mux(b(6), a << 6.U, 0.U)
      val term7 = Mux(b(7), a << 7.U, 0.U)
      
      val term0_reduced = gfReduce(term0, getIrreduciblePolynomial(fieldSize), fieldSize)
      val term1_reduced = gfReduce(term1, getIrreduciblePolynomial(fieldSize), fieldSize)
      val term2_reduced = gfReduce(term2, getIrreduciblePolynomial(fieldSize), fieldSize)
      val term3_reduced = gfReduce(term3, getIrreduciblePolynomial(fieldSize), fieldSize)
      val term4_reduced = gfReduce(term4, getIrreduciblePolynomial(fieldSize), fieldSize)
      val term5_reduced = gfReduce(term5, getIrreduciblePolynomial(fieldSize), fieldSize)
      val term6_reduced = gfReduce(term6, getIrreduciblePolynomial(fieldSize), fieldSize)
      val term7_reduced = gfReduce(term7, getIrreduciblePolynomial(fieldSize), fieldSize)
      // XOR all terms together
      result := term0_reduced ^ term1_reduced ^ term2_reduced ^ term3_reduced ^ term4_reduced ^ term5_reduced ^ term6_reduced ^ term7_reduced
      // val extendedTempResult = tempResult.zext(1) // Extend to 16 bits
      // result := gfReduce(extendedTempResult, getIrreduciblePolynomial(fieldSize), fieldSize)
      //* Todo: Implement this properly to check it there is a problem with the reduction
    }
    
    result
  }
  
  /** Galois Field inverse using Extended Euclidean Algorithm
    * 
    * @param a Element to find inverse of
    * @param fieldSize Size of the field
    * @return Inverse of a in GF(2^fieldSize)
    */
  def gfInverse(a: UInt, fieldSize: Int): UInt = {
    val irreduciblePoly = getIrreduciblePolynomial(fieldSize)
    
    // Extended Euclidean Algorithm implementation
    // This is a simplified version - in practice, you might want to use
    // lookup tables or more efficient algorithms for larger fields
    val result = Wire(UInt(fieldSize.W))
    
    // For now, return a simple implementation
    // In a real implementation, you'd use the Extended Euclidean Algorithm
    when(a === 0.U) {
      result := 0.U  // No inverse for zero
    }.otherwise {
      result := gfPower(a, (fieldSize - 2).U, fieldSize)
    }
    result
  }
  
  /** Galois Field exponentiation using state machine
    * Simple approach: multiply base by itself 'exponent' times
    * 
    * @param base Base element
    * @param exponent Exponent
    * @param fieldSize Size of the field
    * @return Result of base^exponent in GF(2^fieldSize)
    */
  def gfPower(base: UInt, exponent: UInt, fieldSize: Int): UInt = {
    val result = Wire(UInt(fieldSize.W))
    
    // State machine registers
    val state = RegInit(0.U(2.W))  // 0: IDLE, 1: COMPUTING, 2: DONE
    val temp_result = RegInit(1.U(fieldSize.W))
    val original_base = RegInit(0.U(fieldSize.W))
    val temp_exp = RegInit(0.U(fieldSize.W))
    
    // Default outputs
    result := temp_result
    
    // State machine
    switch(state) {
      is(0.U) { // IDLE state
        // Initialize for new computation
        temp_result := 1.U
        original_base := base
        temp_exp := exponent
        state := 1.U
      }
      
      is(1.U) { // COMPUTING state
        // Check if we're done (exponent reached 0)
        when(temp_exp === 0.U) {
          state := 2.U
        }.otherwise {
          // Multiply result by original base once
          temp_result := gfMultiply(temp_result, original_base, fieldSize)
          // Reduce exponent by 1
          temp_exp := temp_exp - 1.U
        }
      }
      
      is(2.U) { // DONE state
        // Handle special cases and output result
        when(exponent === 0.U) {
          result := 1.U
        }.elsewhen(base === 0.U) {
          result := 0.U
        }.otherwise {
          result := temp_result
        }
        
        // Stay in DONE state until new computation starts
        // (This will be controlled by the calling logic)
      }
    }
    
    result
  }
  
  /** Reduce polynomial modulo irreducible polynomial
    * 
    * This function iteratively reduces a polynomial by:
    * 1. Finding the highest (most significant) non-zero bit position
    * 2. If that bit position is outside the field (>= fieldSize), reducing it
    *    by XORing with the irreducible polynomial shifted appropriately
    * 3. Repeating until all bits are within the field size
    * 
    * Sequential implementation: finds MSB first, then reduces if needed
    * 
    * @param poly Polynomial to reduce
    * @param irreduciblePoly Irreducible polynomial
    * @param fieldSize Size of the field
    * @return Reduced polynomial
    */
 def gfReduce(poly: UInt, irreduciblePoly: UInt, fieldSize: Int): UInt = {
  reduce_temp_poly := poly
  reduce_irreducible := irreduciblePoly
  reduce_requested := true.B  // Request reduction
  
  reduce_result  // Return current result (may be from previous operation)
}

switch(reduce_state) {
  is(0.U) { // IDLE
    when(reduce_requested) {
      reduce_state := 1.U
      reduce_requested := false.B
    }
  }
  
is(1.U) { // COMPUTING
    when(reduce_temp_poly === 0.U) {
        reduce_result := 0.U
        reduce_state := 2.U
    }.otherwise {
        // Find the position of the highest set bit
        val msb = PriorityEncoder(Reverse(reduce_temp_poly))
        val polyWidth = (fieldSize * 2).U
        val actualMsb = polyWidth - 1.U - msb  // Convert to actual MSB position
        
        when(actualMsb < fieldSize.U) {
            // Already reduced
            reduce_result := reduce_temp_poly(fieldSize - 1, 0)
            reduce_state := 2.U
        }.otherwise {
            // Need to reduce
            val shiftAmount = actualMsb - fieldSize.U
            reduce_temp_poly := reduce_temp_poly ^ (reduce_irreducible << shiftAmount)
            // Stay in COMPUTING
        }
    }
}
  
  is(2.U) { // DONE
    reduce_state := 0.U  // Return to IDLE
  }
}
 
//   def gfReduce(poly: UInt, irreduciblePoly: UInt, fieldSize: Int): UInt = {
//     val result = Wire(UInt(fieldSize.W))
    
//     // Combinational reduction - check each bit position
//     val temp = Wire(Vec(fieldSize + 1, UInt((fieldSize * 2).W)))
//     temp(0) := poly
    
//     for (i <- 0 until fieldSize) {
//         val bitPos = (fieldSize * 2 - 1) - i
//         when(temp(i)(bitPos)) {
//             val shiftAmount = bitPos - fieldSize
//             temp(i + 1) := temp(i) ^ (irreduciblePoly << shiftAmount)
//         }.otherwise {
//             temp(i + 1) := temp(i)
//         }
//     }
    
//     result := temp(fieldSize)(fieldSize - 1, 0)
//     result
// }
  /** Get irreducible polynomial for given field size
    * 
    * @param fieldSize Size of the field
    * @return Irreducible polynomial
    */
  def getIrreduciblePolynomial(fieldSize: Int): UInt = {
    fieldSize match {
      case 8 => "b100011101".U  // x^8 + x^4 + x^3 + x^2 + 1
      case 16 => "b10000000000011011".U  // x^16 + x^5 + x^3 + x + 1
      case 32 => "b100000000000000000000000000101101".U  // x^32 + x^7 + x^3 + x^2 + 1
      case _ => "b100011101".U  // Default to GF(2^8) polynomial
    }
  }
}

