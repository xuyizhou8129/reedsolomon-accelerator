package roccacc

import chisel3._
import chisel3.util._

// Coordinator orchestrates the RS decoder pipeline:
//   1. Run CheckRoots on the received codeword (writes M and S to memory)
//   2. If corrupted: iterate over all k-subsets of column indices (numErrors = 1..t)
//      For each subset: gather active columns of M into baseAcompact, restore S→baseB, run MatSolve
//      Stop on first solvable trial → scatter compact x back to full baseX → report solved
//   3. If no trial succeeds: report failure
//
// Memory layout (all flat, one GF element per word):
//   baseM        : M matrix (numRoots × n words, row-major) — written by CheckRoots, read-only after
//   baseS        : syndrome S (numRoots words)              — written by CheckRoots, never modified
//   baseAcompact : compact trial A (numRoots × t words)     — gather phase writes active columns only
//   baseB        : b copy (numRoots words)                  — restored from baseS before each MatSolve
//   baseXbuf     : compact x output (t words)               — written by MatSolve (numErrReg entries used)
//   baseX        : full error vector (n words)              — scattered from baseXbuf after success
//
// Memory savings vs full baseA: numRoots*(n-t) words per trial (e.g. 4*13=52 words for RS(15,11)).
// MatSolve receives runtime rows=numRoots and cols=numErrReg so it only processes the active submatrix.
//
// io.corrupted and io.solved are valid when io.done pulses.
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

  val io = IO(new Bundle {
    val start        = Input(Bool())
    val busy         = Output(Bool())
    val done         = Output(Bool())
    val corrupted    = Output(Bool())
    val solved       = Output(Bool())
    val coeffs       = Input(Vec(n, UInt(fieldSize.W)))
    val baseM        = Input(UInt(addrWidth.W))
    val baseS        = Input(UInt(addrWidth.W))
    val baseAcompact = Input(UInt(addrWidth.W))
    val baseB        = Input(UInt(addrWidth.W))
    val baseXbuf     = Input(UInt(addrWidth.W))
    val baseX        = Input(UInt(addrWidth.W))
    val memAddr      = Output(UInt(addrWidth.W))
    val memWData     = Output(UInt(fieldSize.W))
    val memRData     = Input(UInt(fieldSize.W))
    val memRead      = Output(Bool())
    val memWrite     = Output(Bool())
    val memReady     = Input(Bool())
  })

  val cr = Module(new CheckRoots(n, roots, addrWidth, fieldSize))
  val ms = Module(new MatSolve(numRoots, t, addrWidth))

  object S extends ChiselEnum {
    val sIdle,
        sCRStart,      // one-cycle pulse: assert CheckRoots start
        sCRWait,       // wait for CheckRoots done; forward write-only memory port
        sGatherRd,     // read M[copyI][combReg(copyCI)] from memory
        sGatherWr,     // write baseAcompact[copyI][copyCI] = rdVal
        sRestoreBRd,   // read S[copyI] from baseS
        sRestoreBWr,   // write S[copyI] to baseB
        sMSStart,      // one-cycle pulse: assert MatSolve start
        sMSWait,       // wait for MatSolve done; forward full read/write memory port
        sScatterRd,    // read baseXbuf[copyCI] (compact x from MatSolve)
        sScatterWr,    // write baseX[combReg(copyCI)] = rdVal (scatter to full error vector)
        sAdvComb,      // advance combination index or error count
        sDone = Value  // pulse io.done; latch corrupted/solved
  }
  import S._

  val state = RegInit(sIdle)

  val corruptedReg     = RegInit(false.B)
  val solvedReg        = RegInit(false.B)
  val baseMReg         = Reg(UInt(addrWidth.W))
  val baseSReg         = Reg(UInt(addrWidth.W))
  val baseAcompactReg  = Reg(UInt(addrWidth.W))
  val baseBReg         = Reg(UInt(addrWidth.W))
  val baseXbufReg      = Reg(UInt(addrWidth.W))
  val baseXReg         = Reg(UInt(addrWidth.W))

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

  // ---- Default IO outputs ----
  io.memAddr  := 0.U
  io.memWData := 0.U
  io.memRead  := false.B
  io.memWrite := false.B
  io.done      := false.B
  io.busy      := state =/= sIdle
  io.corrupted := corruptedReg
  io.solved    := solvedReg

  // ---- CheckRoots sub-module (write-only memory port forwarded in sCRWait) ----
  cr.io.start    := false.B
  cr.io.coeffs   := io.coeffs
  cr.io.baseM    := baseMReg
  cr.io.baseS    := baseSReg
  cr.io.memReady := false.B

  // ---- MatSolve sub-module (full memory port forwarded in sMSWait) ----
  ms.io.start    := false.B
  ms.io.rows     := numRoots.U
  ms.io.cols     := numErrReg
  ms.io.baseA    := baseAcompactReg
  ms.io.baseB    := baseBReg
  ms.io.baseX    := baseXbufReg
  ms.io.memRData := 0.U
  ms.io.memReady := false.B

  // ---- FSM ----
  switch(state) {

    is(sIdle) {
      when(io.start) {
        baseMReg        := io.baseM
        baseSReg        := io.baseS
        baseAcompactReg := io.baseAcompact
        baseBReg        := io.baseB
        baseXbufReg     := io.baseXbuf
        baseXReg        := io.baseX
        corruptedReg    := false.B
        solvedReg       := false.B
        state           := sCRStart
        printf("(Coordinator) start\n")
      }
    }

    is(sCRStart) {
      cr.io.start := true.B
      state       := sCRWait
    }

    is(sCRWait) {
      // Forward CheckRoots write-only memory port
      io.memAddr     := cr.io.memAddr
      io.memWData    := cr.io.memWData
      io.memWrite    := cr.io.memWrite
      cr.io.memReady := io.memReady

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
      io.memAddr := baseMReg + copyI * n.U + combReg(copyCI)
      io.memRead := true.B
      when(io.memReady) {
        rdVal := io.memRData
        state := sGatherWr
      }
    }

    is(sGatherWr) {
      io.memAddr  := baseAcompactReg + copyI * numErrReg + copyCI
      io.memWData := rdVal
      io.memWrite := true.B
      when(io.memReady) {
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
          // Gather done; restore b from master syndrome
          copyI := 0.U
          state := sRestoreBRd
        }
      }
    }

    // Copy S[copyI] from baseS to baseB (restore b for this MatSolve trial)
    is(sRestoreBRd) {
      io.memAddr := baseSReg + copyI
      io.memRead := true.B
      when(io.memReady) {
        rdVal := io.memRData
        state := sRestoreBWr
      }
    }

    is(sRestoreBWr) {
      io.memAddr  := baseBReg + copyI
      io.memWData := rdVal
      io.memWrite := true.B
      when(io.memReady) {
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
      // Forward MatSolve full read/write memory port
      io.memAddr     := ms.io.memAddr
      io.memWData    := ms.io.memWData
      io.memRead     := ms.io.memRead
      io.memWrite    := ms.io.memWrite
      ms.io.memRData := io.memRData
      ms.io.memReady := io.memReady

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

    // Scatter compact x (baseXbuf[copyCI]) to full error vector (baseX[combReg(copyCI)])
    is(sScatterRd) {
      io.memAddr := baseXbufReg + copyCI
      io.memRead := true.B
      when(io.memReady) {
        rdVal := io.memRData
        state := sScatterWr
      }
    }

    is(sScatterWr) {
      io.memAddr  := baseXReg + combReg(copyCI)
      io.memWData := rdVal
      io.memWrite := true.B
      when(io.memReady) {
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
      // For position i: combReg(i) < n - numErrReg + i  (max value for that position)
      // Implemented unrolled for fixed t (elaboration-time constant)
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
          // Reset tail: new combReg(1) = new combReg(0) + 1 = old combReg(0) + 2
          when(numErrReg >= 2.U) {
            combReg(1) := combReg(0) + 2.U
          }
        }
        copyI := 0.U; copyCI := 0.U
        state := sGatherRd
      }.elsewhen(canNextErr) {
        numErrReg := numErrReg + 1.U
        // Initialize first combo for new error count: [0, 1, ..., newNumErr-1]
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
