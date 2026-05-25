package roccacc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile.CoreModule

object GFOperations {
  val DEFAULT_FIELD_SIZE = 8

  val SZ_GF_FN = 4.W

  def FN_X   = BitPat("b????")
  def FN_ADD = BitPat("b0000")
  def FN_MUL = BitPat("b0001")
  def FN_DIV = BitPat("b0010")
  def FN_POW = BitPat("b0011")
  def FN_INV = BitPat("b0100")
  def FN_RED = BitPat("b0101")
  def FN_GIP = BitPat("b0110")

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
    val out  = Valid(UInt(fieldSize.W))
  })

  private val reductionMatrix = GFReduceGen.buildReductionMatrix(fieldSize, GFReduceGen.GF256_POLY)

  val outBits = Wire(Vec(fieldSize, Bool()))
  for (j <- 0 until fieldSize) {
    outBits(j) := reductionMatrix(j).map(i => io.in1.bits(i)).reduce(_ ^ _)
  }

  io.in1.ready := true.B
  io.out.valid := io.in1.valid
  io.out.bits  := outBits.asUInt
}

class GFAdd(fieldSize: Int = GFOperations.DEFAULT_FIELD_SIZE) extends Module {
  val io = IO(new Bundle {
    val in1 = Flipped(Decoupled(UInt((2 * fieldSize).W)))
    val in2 = Flipped(Decoupled(UInt((2 * fieldSize).W)))
    val out  = Valid(UInt(fieldSize.W))
  })

  val reducer1 = Module(new GFReduce(fieldSize))
  val reducer2 = Module(new GFReduce(fieldSize))

  // --- Datapath ---
  reducer1.io.in1.bits  := io.in1.bits
  reducer1.io.in1.valid := io.in1.valid
  reducer2.io.in1.bits  := io.in2.bits
  reducer2.io.in1.valid := io.in2.valid

  val result_reg = RegInit(0.U(fieldSize.W))
  val valid_reg  = RegInit(false.B)

  // 1-cycle pipeline to match the sAddS/sAddW Send/Wait caller pattern;
  // a purely combinational path would de-assert out.valid before callers reach sAddW.
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
    val out  = Valid(UInt(fieldSize.W))
  })

  object MulState extends ChiselEnum {
    val idle, mul, done = Value
  }

  val mul_state  = RegInit(MulState.idle)
  val aReg       = RegInit(0.U(fieldSize.W))
  val bReg       = RegInit(0.U(fieldSize.W))
  val productReg = RegInit(0.U((2 * fieldSize).W))

  val inReducer1 = Module(new GFReduce(fieldSize))
  val inReducer2 = Module(new GFReduce(fieldSize))
  val outReducer = Module(new GFReduce(fieldSize))

  // --- Datapath (outside FSM) ---
  // inReducer1/2: fold wide inputs to fieldSize bits; bits wired here, valid pulsed in FSM
  inReducer1.io.in1.bits  := io.in1.bits
  inReducer2.io.in1.bits  := io.in2.bits
  inReducer1.io.in1.valid := false.B
  inReducer2.io.in1.valid := false.B

  // outReducer: reduce productReg; valid exactly when done state drives io.out
  outReducer.io.in1.bits  := productReg
  outReducer.io.in1.valid := mul_state === MulState.done

  io.in1.ready := mul_state === MulState.idle
  io.in2.ready := mul_state === MulState.idle
  io.out.valid := mul_state === MulState.done
  io.out.bits  := outReducer.io.out.bits

  // Combinational carry-less multiply over registered operands (cycle 1 → productReg)
  val width    = 2 * fieldSize
  val aWide    = Wire(UInt(width.W))
  aWide := aReg
  val clProduct = Wire(UInt(width.W))
  clProduct := (0 until fieldSize).map { i =>
    Mux(bReg(i), (aWide << i)(width - 1, 0), 0.U(width.W))
  }.reduce(_ ^ _)

  // --- Control (inside FSM) ---
  switch(mul_state) {
    is(MulState.idle) {
      when(io.in1.valid && io.in2.valid) {
        inReducer1.io.in1.valid := true.B
        inReducer2.io.in1.valid := true.B
        aReg      := inReducer1.io.out.bits
        bReg      := inReducer2.io.out.bits
        mul_state := MulState.mul
      }
    }
    is(MulState.mul) {
      productReg := clProduct
      mul_state  := MulState.done
    }
    is(MulState.done) {
      mul_state := MulState.idle
    }
  }
}

class GFPower(fieldSize: Int = GFOperations.DEFAULT_FIELD_SIZE) extends Module {
  val io = IO(new Bundle {
    val in1 = Flipped(Decoupled(UInt((2 * fieldSize).W)))
    val in2 = Flipped(Decoupled(UInt((2 * fieldSize).W)))
    val out  = Valid(UInt(fieldSize.W))
  })

  object PowerState extends ChiselEnum {
    val idle, computing, done = Value
  }

  val power_state      = RegInit(PowerState.idle)
  val load_inputs_done = RegInit(false.B)
  val acc_val          = RegInit(1.U(fieldSize.W))
  val acc_count        = RegInit(0.U((2 * fieldSize).W))
  val storedinput1     = RegInit(0.U((2 * fieldSize).W))
  val storedinput2     = RegInit(0.U((2 * fieldSize).W))

  val multiplier1 = Module(new GFMul(fieldSize))

  // --- Datapath (outside FSM) ---
  multiplier1.io.in1.bits  := 0.U((2 * fieldSize).W)
  multiplier1.io.in1.valid := false.B
  multiplier1.io.in2.bits  := 0.U((2 * fieldSize).W)
  multiplier1.io.in2.valid := false.B

  io.in1.ready := power_state === PowerState.idle
  io.in2.ready := power_state === PowerState.idle
  io.out.valid := power_state === PowerState.done
  io.out.bits  := acc_val

  // --- Control (inside FSM) ---
  switch(power_state) {
    is(PowerState.idle) {
      load_inputs_done := false.B
      when(io.in1.valid && io.in2.valid) {
        power_state  := PowerState.computing
        storedinput1 := io.in1.bits
        storedinput2 := io.in2.bits
      }
    }
    is(PowerState.computing) {
      when(!load_inputs_done && multiplier1.io.in1.ready && multiplier1.io.in2.ready) {
        multiplier1.io.in1.bits  := storedinput1
        multiplier1.io.in1.valid := true.B
        multiplier1.io.in2.bits  := storedinput1
        multiplier1.io.in2.valid := true.B
        load_inputs_done := true.B
        acc_val          := 1.U(fieldSize.W)
        acc_count        := 2.U((2 * fieldSize).W)
      }.elsewhen(load_inputs_done && multiplier1.io.in1.ready && multiplier1.io.in2.ready) {
        multiplier1.io.in1.bits  := storedinput1
        multiplier1.io.in1.valid := true.B
        multiplier1.io.in2.bits  := acc_val
        multiplier1.io.in2.valid := true.B
      }
      when(acc_count < storedinput2 && multiplier1.io.out.valid) {
        acc_val   := multiplier1.io.out.bits
        acc_count := acc_count + 1.U
      }.elsewhen(multiplier1.io.out.valid) {
        acc_val     := multiplier1.io.out.bits
        power_state := PowerState.done
      }
    }
    is(PowerState.done) {
      power_state      := PowerState.idle
      acc_val          := 0.U(fieldSize.W)
      acc_count        := 0.U((2 * fieldSize).W)
      load_inputs_done := false.B
      storedinput1     := 0.U((2 * fieldSize).W)
      storedinput2     := 0.U((2 * fieldSize).W)
    }
  }
}

class GFDiv(fieldSize: Int = GFOperations.DEFAULT_FIELD_SIZE) extends Module {
  val io = IO(new Bundle {
    val in1 = Flipped(Decoupled(UInt((2 * fieldSize).W)))
    val in2 = Flipped(Decoupled(UInt((2 * fieldSize).W)))
    val out  = Valid(UInt(fieldSize.W))
  })

  object DivState extends ChiselEnum {
    val idle, exponentiation, multiplication, done = Value
  }

  val div_state    = RegInit(DivState.idle)
  val inv_result   = RegInit(0.U(fieldSize.W))
  val div_result   = RegInit(0.U(fieldSize.W))
  val storedinput1 = RegInit(0.U((2 * fieldSize).W))
  val storedinput2 = RegInit(0.U((2 * fieldSize).W))

  val multiplier1    = Module(new GFMul(fieldSize))
  val exponentiator1 = Module(new GFPower(fieldSize))

  // --- Datapath (outside FSM) ---
  multiplier1.io.in1.bits    := 0.U((2 * fieldSize).W)
  multiplier1.io.in1.valid   := false.B
  multiplier1.io.in2.bits    := 0.U((2 * fieldSize).W)
  multiplier1.io.in2.valid   := false.B
  exponentiator1.io.in1.bits  := 0.U((2 * fieldSize).W)
  exponentiator1.io.in1.valid := false.B
  exponentiator1.io.in2.bits  := 0.U((2 * fieldSize).W)
  exponentiator1.io.in2.valid := false.B

  io.in1.ready := div_state === DivState.idle
  io.in2.ready := div_state === DivState.idle
  io.out.valid := div_state === DivState.done
  io.out.bits  := div_result

  // --- Control (inside FSM) ---
  switch(div_state) {
    is(DivState.idle) {
      div_result := 0.U(fieldSize.W)
      when(io.in1.valid && io.in2.valid) {
        div_state    := DivState.exponentiation
        storedinput1 := io.in1.bits
        storedinput2 := io.in2.bits
      }
    }
    is(DivState.exponentiation) {
      when(exponentiator1.io.in1.ready && exponentiator1.io.in2.ready) {
        exponentiator1.io.in1.bits  := storedinput2
        exponentiator1.io.in1.valid := true.B
        exponentiator1.io.in2.bits  := ((BigInt(1) << fieldSize) - 2).U((2 * fieldSize).W)
        exponentiator1.io.in2.valid := true.B
      }
      when(exponentiator1.io.out.valid) {
        inv_result := exponentiator1.io.out.bits
        div_state  := DivState.multiplication
      }
    }
    is(DivState.multiplication) {
      when(multiplier1.io.in1.ready && multiplier1.io.in2.ready) {
        multiplier1.io.in1.bits  := inv_result
        multiplier1.io.in1.valid := true.B
        multiplier1.io.in2.bits  := storedinput1
        multiplier1.io.in2.valid := true.B
      }
      when(multiplier1.io.out.valid) {
        div_result := multiplier1.io.out.bits
        div_state  := DivState.done
      }
    }
    is(DivState.done) {
      div_state    := DivState.idle
      inv_result   := 0.U(fieldSize.W)
      div_result   := 0.U(fieldSize.W)
      storedinput1 := 0.U((2 * fieldSize).W)
      storedinput2 := 0.U((2 * fieldSize).W)
    }
  }
}
