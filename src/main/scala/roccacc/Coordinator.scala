package roccacc

import chisel3._
import chisel3.util._

// Coordinator orchestrates the RS decoder pipeline:
//   1. Run CheckRoots on the received codeword (writes M and S to internal scratchpad)
//   2. If corrupted: iterate over all k-subsets of column indices (numErrors = 1..t)
//      For each subset: gather active columns of M into baseAcompact, restore S→baseB, run MatSolve
//      Stop on first solvable trial → scatter compact x to errorVecReg → report solved
//   3. If no trial succeeds: report failure
//
// Internal scratchpad layout (one GF element per word, all addresses compile-time constants):
//   baseM        : M matrix (numRoots × n words, row-major) — written by CheckRoots, read-only after
//   baseS        : syndrome S (numRoots words)              — written by CheckRoots, never modified
//   baseAcompact : compact trial A (numRoots × t words)     — gather phase writes active columns only
//   baseB        : b copy (numRoots words)                  — restored from baseS before each MatSolve
//   baseXbuf     : compact x output (t words)               — written by MatSolve (numErrReg entries used)
//   baseX        : full error vector (n words)              — scattered from baseXbuf after success
//
// io.corrupted, io.solved, and io.errorVec are valid when io.done pulses.
class Coordinator(
  n:         Int      = 15,
  k:         Int      = 11,
  roots:     Seq[Int] = CheckRoots.DEFAULT_ROOTS,
  addrWidth: Int      = 32,
  fieldSize: Int      = GFOperations.DEFAULT_FIELD_SIZE
) extends Module {
  require((n - k) % 2 == 0, "n-k must be even for RS decoding")
  val numRoots = roots.length
  val t = (n - k) / 2

  // Compile-time scratchpad memory layout
  val baseM        = 0
  val baseS        = numRoots * n
  val baseAcompact = baseS + numRoots
  val baseB        = baseAcompact + numRoots * t
  val baseXbuf     = baseB + numRoots
  val baseX        = baseXbuf + t
  val memSize      = baseX + n

  val io = IO(new Bundle {
    val start     = Input(Bool())
    val busy      = Output(Bool())
    val done      = Output(Bool())
    val corrupted = Output(Bool())
    val solved    = Output(Bool())
    val coeffs    = Input(Vec(n, UInt(fieldSize.W)))
    val errorVec  = Output(Vec(n, UInt(fieldSize.W)))
  })

  val scratchpad = Module(new Matrix(memSize, addrWidth, fieldSize))
  val cr = Module(new CheckRoots(n, roots, addrWidth, fieldSize))
  val ms = Module(new MatSolve(numRoots, t, addrWidth))

  object S extends ChiselEnum {
    val sIdle,
        sCRStart,      // one-cycle pulse: assert CheckRoots start
        sCRWait,       // wait for CheckRoots done; forward write-only memory port
        sGatherRd,     // read M[copyI][combReg(copyCI)] from scratchpad
        sGatherWr,     // write baseAcompact[copyI][copyCI] = rdVal
        sRestoreBRd,   // read S[copyI] from baseS
        sRestoreBWr,   // write S[copyI] to baseB
        sMSStart,      // one-cycle pulse: assert MatSolve start
        sMSWait,       // wait for MatSolve done; forward full read/write memory port
        sScatterRd,    // read baseXbuf[copyCI] (compact x from MatSolve)
        sScatterWr,    // write errorVecReg[combReg(copyCI)] = rdVal (scatter to full error vector)
        sAdvComb,      // advance combination index or error count
        sDone = Value  // pulse io.done; latch corrupted/solved
  }
  import S._

  val state = RegInit(sIdle)

  val corruptedReg = RegInit(false.B)
  val solvedReg    = RegInit(false.B)

  // numErrReg: current trial error count (1..t)
  val numErrReg = Reg(UInt(log2Ceil(t + 2).W))
  // combReg: current combination (t entries; only numErrReg are valid)
  val combReg   = Reg(Vec(t, UInt(log2Ceil(n + 1).W)))

  // copyI: row index during gather/restore/scatter phases (0..numRoots-1)
  val copyI  = Reg(UInt(log2Ceil(numRoots + 1).W))
  // copyCI: combo column index during gather and scatter phases (0..t-1)
  val copyCI = Reg(UInt(log2Ceil(t + 1).W))
  // rdVal: temporary read value
  val rdVal  = Reg(UInt(fieldSize.W))

  // errorVecReg: full n-element error vector latched during scatter phase
  val errorVecReg = RegInit(VecInit(Seq.fill(n)(0.U(fieldSize.W))))

  // ---- Default IO outputs ----
  io.done      := false.B
  io.busy      := state =/= sIdle
  io.corrupted := corruptedReg
  io.solved    := solvedReg
  io.errorVec  := errorVecReg

  // ---- Default scratchpad connections ----
  scratchpad.io.memAddr  := 0.U
  scratchpad.io.memWData := 0.U
  scratchpad.io.memRead  := false.B
  scratchpad.io.memWrite := false.B

  // ---- CheckRoots sub-module (write-only memory port forwarded in sCRWait) ----
  cr.io.start    := false.B
  cr.io.coeffs   := io.coeffs
  cr.io.baseM    := baseM.U
  cr.io.baseS    := baseS.U
  cr.io.memReady := false.B

  // ---- MatSolve sub-module (full memory port forwarded in sMSWait) ----
  ms.io.start    := false.B
  ms.io.rows     := numRoots.U
  ms.io.cols     := numErrReg
  ms.io.baseA    := baseAcompact.U
  ms.io.baseB    := baseB.U
  ms.io.baseX    := baseXbuf.U
  ms.io.memRData := 0.U
  ms.io.memReady := false.B

  // ---- FSM ----
  switch(state) {

    is(sIdle) {
      when(io.start) {
        corruptedReg := false.B
        solvedReg    := false.B
        for (i <- 0 until n) { errorVecReg(i) := 0.U }
        state        := sCRStart
        printf("(Coordinator) start\n")
      }
    }

    is(sCRStart) {
      cr.io.start := true.B
      state       := sCRWait
    }

    is(sCRWait) {
      // Forward CheckRoots write-only memory port to scratchpad
      scratchpad.io.memAddr  := cr.io.memAddr
      scratchpad.io.memWData := cr.io.memWData
      scratchpad.io.memWrite := cr.io.memWrite
      cr.io.memReady         := scratchpad.io.memReady

      when(cr.io.done) {
        corruptedReg := cr.io.corrupted
        when(!cr.io.corrupted) {
          printf("(Coordinator) clean codeword\n")
          state := sDone
        }.otherwise {
          printf("(Coordinator) corrupted -- starting trial loop\n")
          numErrReg := 1.U
          for (i <- 0 until t) { combReg(i) := i.U }
          copyI  := 0.U
          copyCI := 0.U
          state  := sGatherRd
        }
      }
    }

    // Gather: read M[copyI][combReg(copyCI)] and write to baseAcompact[copyI][copyCI]
    is(sGatherRd) {
      scratchpad.io.memAddr := baseM.U + copyI * n.U + combReg(copyCI)
      scratchpad.io.memRead := true.B
      when(scratchpad.io.memReady) {
        rdVal := scratchpad.io.memRData
        state := sGatherWr
      }
    }

    is(sGatherWr) {
      scratchpad.io.memAddr  := baseAcompact.U + copyI * numErrReg + copyCI
      scratchpad.io.memWData := rdVal
      scratchpad.io.memWrite := true.B
      when(scratchpad.io.memReady) {
        val nextCI = copyCI + 1.U
        val nextI  = copyI + 1.U
        when(nextCI < numErrReg) {
          copyCI := nextCI
          state  := sGatherRd
        }.elsewhen(nextI < numRoots.U) {
          copyCI := 0.U
          copyI  := nextI
          state  := sGatherRd
        }.otherwise {
          copyI := 0.U
          state := sRestoreBRd
        }
      }
    }

    // Copy S[copyI] from baseS to baseB (restore b for this MatSolve trial)
    is(sRestoreBRd) {
      scratchpad.io.memAddr := baseS.U + copyI
      scratchpad.io.memRead := true.B
      when(scratchpad.io.memReady) {
        rdVal := scratchpad.io.memRData
        state := sRestoreBWr
      }
    }

    is(sRestoreBWr) {
      scratchpad.io.memAddr  := baseB.U + copyI
      scratchpad.io.memWData := rdVal
      scratchpad.io.memWrite := true.B
      when(scratchpad.io.memReady) {
        val nextI = copyI + 1.U
        when(nextI < numRoots.U) {
          copyI := nextI
          state := sRestoreBRd
        }.otherwise {
          state := sMSStart
        }
      }
    }

    is(sMSStart) {
      ms.io.start := true.B
      state       := sMSWait
      printf("(Coordinator) starting MatSolve, numErr=%d combo=[%d,%d]\n",
             numErrReg, combReg(0), if (t >= 2) combReg(1) else 0.U)
    }

    is(sMSWait) {
      // Forward MatSolve full read/write memory port to scratchpad
      scratchpad.io.memAddr  := ms.io.memAddr
      scratchpad.io.memWData := ms.io.memWData
      scratchpad.io.memRead  := ms.io.memRead
      scratchpad.io.memWrite := ms.io.memWrite
      ms.io.memRData         := scratchpad.io.memRData
      ms.io.memReady         := scratchpad.io.memReady

      when(ms.io.done) {
        when(!ms.io.unsolvable) {
          printf("(Coordinator) MatSolve succeeded\n")
          solvedReg := true.B
          copyCI    := 0.U
          state     := sScatterRd
        }.otherwise {
          printf("(Coordinator) MatSolve unsolvable -- trying next combo\n")
          state := sAdvComb
        }
      }
    }

    // Scatter compact x (baseXbuf[copyCI]) to full error vector (errorVecReg[combReg(copyCI)])
    is(sScatterRd) {
      scratchpad.io.memAddr := baseXbuf.U + copyCI
      scratchpad.io.memRead := true.B
      when(scratchpad.io.memReady) {
        rdVal := scratchpad.io.memRData
        state := sScatterWr
      }
    }

    is(sScatterWr) {
      // Also write to scratchpad baseX region for completeness
      scratchpad.io.memAddr  := baseX.U + combReg(copyCI)
      scratchpad.io.memWData := rdVal
      scratchpad.io.memWrite := true.B
      when(scratchpad.io.memReady) {
        errorVecReg(combReg(copyCI)) := rdVal
        val nextCI = copyCI + 1.U
        when(nextCI < numErrReg) {
          copyCI := nextCI
          state  := sScatterRd
        }.otherwise {
          state := sDone
        }
      }
    }

    is(sAdvComb) {
      // Priority: rightmost incrementable position > earlier positions > advance numErrReg > fail
      val canI1      = if (t >= 2) (numErrReg >= 2.U) && (combReg(1) < (n - 1).U) else false.B
      val canI0      = combReg(0) < (n.U - numErrReg)
      val canNextErr = numErrReg < t.U

      when(canI1) {
        combReg(1) := combReg(1) + 1.U
        copyI := 0.U; copyCI := 0.U
        state := sGatherRd
      }.elsewhen(canI0) {
        combReg(0) := combReg(0) + 1.U
        if (t >= 2) {
          when(numErrReg >= 2.U) {
            combReg(1) := combReg(0) + 2.U
          }
        }
        copyI := 0.U; copyCI := 0.U
        state := sGatherRd
      }.elsewhen(canNextErr) {
        numErrReg := numErrReg + 1.U
        for (i <- 0 until t) { combReg(i) := i.U }
        copyI := 0.U; copyCI := 0.U
        state := sGatherRd
      }.otherwise {
        printf("(Coordinator) all combos exhausted -- decoding failure\n")
        state := sDone
      }
    }

    is(sDone) {
      io.done := true.B
      state   := sIdle
      printf("(Coordinator) done corrupted=%b solved=%b\n", corruptedReg, solvedReg)
    }
  }
}
