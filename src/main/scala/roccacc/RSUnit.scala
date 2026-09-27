package roccacc

import chisel3._
import chisel3.util._

/** Operation selector for RSUnit. */
//It is basically a wrapper for the encoder and the decoder
//So that it will be the only unit talking to the RoCC interface
object RSUnit {
  val SZ_OP     = 1
  def OP_ENCODE = 0.U(SZ_OP.W)
  def OP_DECODE = 1.U(SZ_OP.W)
}

class RSUnit(
  n:               Int      = RS_Encoder.DEFAULT_N,
  k:               Int      = RS_Encoder.DEFAULT_K,
  roots:           Seq[Int] = CheckRoots.DEFAULT_ROOTS,
  generatorCoeffs: Seq[Int] = RS_Encoder.DEFAULT_GENERATOR_COEFFS,
  fieldSize:       Int      = GFOperations.DEFAULT_FIELD_SIZE
) extends Module {
    //TODO: mathematically n-k doesnt have to be even, it is an implementation side effect to be fixed later
  require((n - k) % 2 == 0, "n - k must be even for RS codes")
  require(k > 0 && k < n, "need 0 < k < n")

    // Interface contract that talks to the RoCC interface.
  val io = IO(new Bundle {
    val op        = Input(UInt(RSUnit.SZ_OP.W)) //operation dictation
    val start     = Input(Bool()) //start signal
    val in        = Input(Vec(n, UInt(fieldSize.W)))
    val busy      = Output(Bool())
    val done      = Output(Bool())
    val out       = Output(Vec(n, UInt(fieldSize.W)))
    val corrupted = Output(Bool())
    val solved    = Output(Bool())
  })

  // ---- Sub-units ----
  val enc = Module(new RS_Encoder(fieldSize, n, k, generatorCoeffs))
  val dec = Module(new Coordinator(n, k, roots, fieldSize = fieldSize))

  // ---- State ----
  //TODO: currently Encoder and Decoder share the same port so they block each other
  object S extends ChiselEnum {
    val sIdle,
        sEncStart,   // present message to encoder until it accepts
        sEncWait,    // wait for encoder out.valid pulse
        sDecStart,   // one-cycle start pulse to decoder
        sDecWait,    // wait for decoder done pulse
        sDone        // one-cycle done pulse to parent
        = Value
  }
  import S._

  val state = RegInit(sIdle)

  // Inputs latched on accepted start so the parent may change them afterwards
  val inReg  = Reg(Vec(n, UInt(fieldSize.W)))

  // Results held until the next accepted start
  val outReg       = RegInit(VecInit(Seq.fill(n)(0.U(fieldSize.W))))
  val corruptedReg = RegInit(false.B)
  val solvedReg    = RegInit(false.B)

  // ---- Default sub-unit drives ----
  enc.io.message.valid := false.B
  enc.io.message.bits  := VecInit(inReg.take(k))
  dec.io.start         := false.B
  dec.io.coeffs        := inReg

  // ---- Outputs ----
  io.busy      := state =/= sIdle
  io.done      := state === sDone
  io.out       := outReg
  io.corrupted := corruptedReg
  io.solved    := solvedReg

  // ---- Control ----
  switch(state) {
    is(sIdle) {
      when(io.start) {
        inReg        := io.in
        corruptedReg := false.B
        solvedReg    := false.B
        state        := Mux(io.op === RSUnit.OP_DECODE, sDecStart, sEncStart)
      }
    }

    is(sEncStart) {
      enc.io.message.valid := true.B
      when(enc.io.message.fire) {
        state := sEncWait
      }
    }

    is(sEncWait) {
      when(enc.io.out.valid) {
        outReg := enc.io.out.bits
        state  := sDone
      }
    }

    is(sDecStart) {
      dec.io.start := true.B
      state        := sDecWait
    }

    is(sDecWait) {
      when(dec.io.done) {
        outReg       := dec.io.errorVec
        corruptedReg := dec.io.corrupted
        solvedReg    := dec.io.solved
        state        := sDone
      }
    }

    is(sDone) {
      state := sIdle
    }
  }
}
