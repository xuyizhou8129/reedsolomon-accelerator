package roccacc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile.CoreModule

class MatSolveIO(addrWidth: Int, dataWidth: Int) extends Bundle {
  val start      = Input(Bool())
  val done       = Output(Bool())
  val busy       = Output(Bool())
  val unsolvable = Output(Bool())

  val baseA = Input(UInt(addrWidth.W))
  val baseB = Input(UInt(addrWidth.W))
  val baseX = Input(UInt(addrWidth.W))

  val memAddr  = Output(UInt(addrWidth.W))
  val memWData = Output(UInt(dataWidth.W))
  val memRData = Input(UInt(dataWidth.W))
  val memRead  = Output(Bool())
  val memWrite = Output(Bool())
  val memReady = Input(Bool())
}

// MatSolve performs Ax=b by Gaussian elimination in GF(2^8).
// No local copy of the matrix is kept; all data is fetched from and written
// back to the memory scratchpad one element at a time.
// Layout: A at baseA (row-major), b at baseB, x written to baseX.
class MatSolve(rows: Int, cols: Int, addrWidth: Int = 32) extends Module {
  val fs = GFOperations.DEFAULT_FIELD_SIZE
  require(fs == 8, "MatSolve dataWidth tied to GF(2^8) elements in memory")
  val ww = 2 * fs

  val io = IO(new MatSolveIO(addrWidth, fs))

  val gfMul = Module(new GFMul(fs))
  val gfAdd = Module(new GFAdd(fs))
  val gfDiv = Module(new GFDiv(fs))

  gfMul.io.in1.bits := 0.U(ww.W); gfMul.io.in1.valid := false.B
  gfMul.io.in2.bits := 0.U(ww.W); gfMul.io.in2.valid := false.B
  gfAdd.io.in1.bits := 0.U(ww.W); gfAdd.io.in1.valid := false.B
  gfAdd.io.in2.bits := 0.U(ww.W); gfAdd.io.in2.valid := false.B
  gfDiv.io.in1.bits := 0.U(ww.W); gfDiv.io.in1.valid := false.B
  gfDiv.io.in2.bits := 0.U(ww.W); gfDiv.io.in2.valid := false.B

  object S extends ChiselEnum {
    val sIdle,
        // pivot scan
        sFwScan,
        // row swap (pivot != numPivots)
        sFwSwapRdNP, sFwSwapRdPR, sFwSwapWrNP, sFwSwapWrPR,
        // invert pivot element
        sFwInvRd, sFwInvS, sFwInvW,
        // normalize pivot row
        sFwNormRd, sFwNormMulS, sFwNormMulW, sFwNormWr,
        // eliminate rows below pivot
        sFwElimCheckRd,
        sFwElimPivRd, sFwElimMulS, sFwElimMulW,
        sFwElimRowRd, sFwElimAddS, sFwElimAddW, sFwElimWr,
        // advance to next column
        sFwNextCol,
        // consistency check
        sChkRdA, sChkRdB,
        // back-substitution
        sBsSetup, sBsRow, sBsRdAcc,
        sBsColRd, sBsMulS, sBsMulW, sBsAddS, sBsAddW,
        // store solution
        sStoreX,
        sDonePulse = Value
  }
  import S._

  val state = RegInit(sIdle)

  val baseAReg  = Reg(UInt(addrWidth.W))
  val baseBReg  = Reg(UInt(addrWidth.W))
  val baseXReg  = Reg(UInt(addrWidth.W))

  val numPivots = Reg(UInt(log2Ceil(rows + 1).W))
  val colIdx    = Reg(UInt(log2Ceil(cols + 1).W))
  val scanR     = Reg(UInt(log2Ceil(rows + 1).W))
  val pivRow    = Reg(UInt(log2Ceil(math.max(rows, 2)).W))
  val hasPivot  = Reg(Bool())

  val swapJ     = Reg(UInt(log2Ceil(cols + 2).W))
  val swapTmpNP = Reg(UInt(fs.W))

  val invP  = Reg(UInt(fs.W))
  val jNorm = Reg(UInt(log2Ceil(cols + 2).W))

  val elimR = Reg(UInt(log2Ceil(rows + 1).W))
  val elimJ = Reg(UInt(log2Ceil(cols + 2).W))
  val fac   = Reg(UInt(fs.W))
  val prod  = Reg(UInt(fs.W))
  val rdVal = Reg(UInt(fs.W))

  // pivot column index for each pivot row; set during sFwNextCol
  val pivCol = Reg(Vec(rows, UInt(log2Ceil(math.max(cols, 1)).W)))

  val chkR     = Reg(UInt(log2Ceil(rows + 1).W))
  val chkC     = Reg(UInt(log2Ceil(cols + 1).W))
  val allZeroA = Reg(Bool())

  val bsI   = Reg(UInt(log2Ceil(rows + 1).W))
  val bsJ   = Reg(UInt(log2Ceil(rows + 1).W))
  val bsAcc = Reg(UInt(fs.W))

  val xReg      = RegInit(VecInit(Seq.fill(cols)(0.U(fs.W))))
  val storeXIdx = Reg(UInt(log2Ceil(math.max(cols, 1)).W))
  val unsolv    = RegInit(false.B)

  io.memAddr    := 0.U
  io.memWData   := 0.U
  io.memRead    := false.B
  io.memWrite   := false.B
  io.done       := false.B
  io.busy       := state =/= sIdle
  io.unsolvable := unsolv

  def pz(u: UInt): UInt = u.pad(ww)

  // aug[r][j]: for j < cols -> A at baseA + r*cols + j; for j = cols -> b at baseB + r
  def addrA(r: UInt, c: UInt): UInt = baseAReg + r * cols.U + c
  def addrB(r: UInt): UInt          = baseBReg + r
  def augAddr(r: UInt, j: UInt): UInt =
    Mux(j < cols.U, addrA(r, j), addrB(r))

  switch(state) {
    is(sIdle) {
      unsolv := false.B
      when(io.start) {
        baseAReg  := io.baseA
        baseBReg  := io.baseB
        baseXReg  := io.baseX
        numPivots := 0.U
        colIdx    := 0.U
        scanR     := 0.U
        hasPivot  := false.B
        state     := sFwScan
        printf("(MatSolve) start: scratchpad-based computation\n")
      }
    }

    // Scan rows >= numPivots for first non-zero in column colIdx
    is(sFwScan) {
      when(scanR >= rows.U) {
        hasPivot := false.B
        state    := sFwNextCol
      }.otherwise {
        io.memAddr := augAddr(scanR, colIdx)
        io.memRead := true.B
        // Keeps requesting until a valid signal is received
        when(io.memReady) {
          when(io.memRData =/= 0.U) {
            pivRow   := scanR
            hasPivot := true.B
            when(scanR === numPivots) {
              state := sFwInvRd        // pivot already in place, skip swap
            }.otherwise {
              swapJ := 0.U
              state := sFwSwapRdNP
            }
          }.otherwise {
            scanR := scanR + 1.U
          }
        }
      }
    }

    // Swap rows numPivots and pivRow, column by column (j = 0..cols)
    is(sFwSwapRdNP) {
      io.memAddr := augAddr(numPivots, swapJ)
      io.memRead := true.B
      when(io.memReady) {
        swapTmpNP := io.memRData
        state     := sFwSwapRdPR
      }
    }

    is(sFwSwapRdPR) {
      io.memAddr := augAddr(pivRow, swapJ)
      io.memRead := true.B
      when(io.memReady) {
        rdVal := io.memRData
        state := sFwSwapWrNP
      }
    }

    is(sFwSwapWrNP) {
      io.memAddr  := augAddr(numPivots, swapJ)
      io.memWData := rdVal
      io.memWrite := true.B
      when(io.memReady) { state := sFwSwapWrPR }
    }

    is(sFwSwapWrPR) {
      io.memAddr  := augAddr(pivRow, swapJ)
      io.memWData := swapTmpNP
      io.memWrite := true.B
      when(io.memReady) {
        when(swapJ === cols.U) {
          state := sFwInvRd
        }.otherwise {
          swapJ := swapJ + 1.U
          state := sFwSwapRdNP
        }
      }
    }

    // Read aug[numPivots][colIdx] to compute its inverse
    is(sFwInvRd) {
      io.memAddr := augAddr(numPivots, colIdx)
      io.memRead := true.B
      when(io.memReady) {
        rdVal := io.memRData
        state := sFwInvS
      }
    }

    is(sFwInvS) {
      when(gfDiv.io.in1.ready && gfDiv.io.in2.ready) {
        gfDiv.io.in1.bits  := 1.U(ww.W)
        gfDiv.io.in1.valid := true.B
        gfDiv.io.in2.bits  := pz(rdVal)
        gfDiv.io.in2.valid := true.B
        state              := sFwInvW
      }
    }

    is(sFwInvW) {
      when(gfDiv.io.out.valid) {
        invP  := gfDiv.io.out.bits
        jNorm := colIdx
        state := sFwNormRd
      }
    }

    // Normalize pivot row: aug[numPivots][j] *= invP  for j = colIdx..cols
    is(sFwNormRd) {
      io.memAddr := augAddr(numPivots, jNorm)
      io.memRead := true.B
      when(io.memReady) {
        rdVal := io.memRData
        state := sFwNormMulS
      }
    }

    is(sFwNormMulS) {
      when(gfMul.io.in1.ready && gfMul.io.in2.ready) {
        gfMul.io.in1.bits  := pz(rdVal)
        gfMul.io.in1.valid := true.B
        gfMul.io.in2.bits  := pz(invP)
        gfMul.io.in2.valid := true.B
        state              := sFwNormMulW
      }
    }

    is(sFwNormMulW) {
      when(gfMul.io.out.valid) {
        rdVal := gfMul.io.out.bits
        state := sFwNormWr
      }
    }

    is(sFwNormWr) {
      io.memAddr  := augAddr(numPivots, jNorm)
      io.memWData := rdVal
      io.memWrite := true.B
      when(io.memReady) {
        when(jNorm === cols.U) {
          elimR := numPivots + 1.U
          state := sFwElimCheckRd
        }.otherwise {
          jNorm := jNorm + 1.U
          state := sFwNormRd
        }
      }
    }

    // For each row elimR > numPivots with non-zero factor: zero out column colIdx
    // and propagate to all columns j = colIdx..cols
    is(sFwElimCheckRd) {
      when(elimR >= rows.U) {
        state := sFwNextCol
      }.otherwise {
        io.memAddr := augAddr(elimR, colIdx)
        io.memRead := true.B
        when(io.memReady) {
          when(io.memRData === 0.U) {
            elimR := elimR + 1.U
          }.otherwise {
            fac   := io.memRData
            elimJ := colIdx
            state := sFwElimPivRd
          }
        }
      }
    }

    // For each column elimJ: compute fac * aug[numPivots][elimJ], add to aug[elimR][elimJ]
    is(sFwElimPivRd) {
      io.memAddr := augAddr(numPivots, elimJ)
      io.memRead := true.B
      when(io.memReady) {
        rdVal := io.memRData
        state := sFwElimMulS
      }
    }

    is(sFwElimMulS) {
      when(gfMul.io.in1.ready && gfMul.io.in2.ready) {
        gfMul.io.in1.bits  := pz(fac)
        gfMul.io.in1.valid := true.B
        gfMul.io.in2.bits  := pz(rdVal)
        gfMul.io.in2.valid := true.B
        state              := sFwElimMulW
      }
    }

    is(sFwElimMulW) {
      when(gfMul.io.out.valid) {
        prod  := gfMul.io.out.bits
        state := sFwElimRowRd
      }
    }

    is(sFwElimRowRd) {
      io.memAddr := augAddr(elimR, elimJ)
      io.memRead := true.B
      when(io.memReady) {
        rdVal := io.memRData
        state := sFwElimAddS
      }
    }

    is(sFwElimAddS) {
      when(gfAdd.io.in1.ready && gfAdd.io.in2.ready) {
        gfAdd.io.in1.bits  := pz(rdVal)
        gfAdd.io.in1.valid := true.B
        gfAdd.io.in2.bits  := pz(prod)
        gfAdd.io.in2.valid := true.B
        state              := sFwElimAddW
      }
    }

    is(sFwElimAddW) {
      when(gfAdd.io.out.valid) {
        rdVal := gfAdd.io.out.bits
        state := sFwElimWr
      }
    }

    is(sFwElimWr) {
      io.memAddr  := augAddr(elimR, elimJ)
      io.memWData := rdVal
      io.memWrite := true.B
      when(io.memReady) {
        when(elimJ === cols.U) {
          elimR := elimR + 1.U
          state := sFwElimCheckRd
        }.otherwise {
          elimJ := elimJ + 1.U
          state := sFwElimPivRd
        }
      }
    }

    // Record pivot column, increment numPivots, advance to next column or consistency check
    is(sFwNextCol) {
      val newNP = Mux(hasPivot, numPivots + 1.U, numPivots)
      when(hasPivot) {
        pivCol(numPivots) := colIdx
        numPivots         := newNP
      }
      when(colIdx === (cols - 1).U) {
        chkR     := newNP
        chkC     := 0.U
        allZeroA := true.B
        state    := sChkRdA
      }.otherwise {
        colIdx := colIdx + 1.U
        scanR  := newNP
        state  := sFwScan
      }
    }

    // For rows chkR >= numPivots: check all-zero A row with non-zero b => inconsistent
    is(sChkRdA) {
      when(chkR >= rows.U) {
        state := sBsSetup
      }.otherwise {
        io.memAddr := addrA(chkR, chkC)
        io.memRead := true.B
        when(io.memReady) {
          //TODO: Add an early exit if allZeroA is false
          when(io.memRData =/= 0.U) { allZeroA := false.B }
          when(chkC === (cols - 1).U) {
            state := sChkRdB
          }.otherwise {
            chkC := chkC + 1.U
          }
        }
      }
    }

    is(sChkRdB) {
      io.memAddr := addrB(chkR)
      io.memRead := true.B
      when(io.memReady) {
        when(allZeroA && io.memRData =/= 0.U) {
          unsolv := true.B
          state  := sDonePulse
        }.otherwise {
          chkR     := chkR + 1.U
          chkC     := 0.U
          allZeroA := true.B
          state    := sChkRdA
        }
      }
    }

    is(sBsSetup) {
      when(numPivots === 0.U) {
        storeXIdx := 0.U
        state     := sStoreX
      }.otherwise {
        bsI   := numPivots - 1.U
        state := sBsRow
      }
    }

    // Back-substitution: for row bsI (descending), compute x[pivCol[bsI]]
    is(sBsRow) {
      bsJ   := bsI + 1.U
      state := sBsRdAcc
    }

    // Read b value for row bsI; if no further terms, set x directly
    is(sBsRdAcc) {
      io.memAddr := addrB(bsI)
      io.memRead := true.B
      when(io.memReady) {
        val bVal = io.memRData
        bsAcc := bVal
        when(bsJ >= numPivots) {
          xReg(pivCol(bsI)) := bVal
          when(bsI === 0.U) {
            storeXIdx := 0.U
            state     := sStoreX
          }.otherwise {
            bsI   := bsI - 1.U
            state := sBsRow
          }
        }.otherwise {
          state := sBsColRd
        }
      }
    }

    // For each bsJ > bsI: bsAcc ^= aug[bsI][pivCol[bsJ]] * x[pivCol[bsJ]]
    is(sBsColRd) {
      io.memAddr := addrA(bsI, pivCol(bsJ))
      io.memRead := true.B
      when(io.memReady) {
        rdVal := io.memRData
        state := sBsMulS
      }
    }

    is(sBsMulS) {
      when(gfMul.io.in1.ready && gfMul.io.in2.ready) {
        gfMul.io.in1.bits  := pz(rdVal)
        gfMul.io.in1.valid := true.B
        gfMul.io.in2.bits  := pz(xReg(pivCol(bsJ)))
        gfMul.io.in2.valid := true.B
        state              := sBsMulW
      }
    }

    is(sBsMulW) {
      when(gfMul.io.out.valid) {
        prod  := gfMul.io.out.bits
        state := sBsAddS
      }
    }

    is(sBsAddS) {
      when(gfAdd.io.in1.ready && gfAdd.io.in2.ready) {
        gfAdd.io.in1.bits  := pz(bsAcc)
        gfAdd.io.in1.valid := true.B
        gfAdd.io.in2.bits  := pz(prod)
        gfAdd.io.in2.valid := true.B
        state              := sBsAddW
      }
    }

    is(sBsAddW) {
      when(gfAdd.io.out.valid) {
        val newAcc = gfAdd.io.out.bits
        bsAcc     := newAcc
        val nextJ  = bsJ + 1.U
        when(nextJ >= numPivots) {
          xReg(pivCol(bsI)) := newAcc
          when(bsI === 0.U) {
            storeXIdx := 0.U
            state     := sStoreX
          }.otherwise {
            bsI   := bsI - 1.U
            state := sBsRow
          }
        }.otherwise {
          bsJ   := nextJ
          state := sBsColRd
        }
      }
    }

    is(sStoreX) {
      io.memAddr  := baseXReg + storeXIdx
      io.memWData := xReg(storeXIdx)
      io.memWrite := true.B
      when(io.memReady) {
        when(storeXIdx === (cols - 1).U) {
          state := sDonePulse
        }.otherwise {
          storeXIdx := storeXIdx + 1.U
        }
      }
    }

    is(sDonePulse) {
      io.done := true.B
      state   := sIdle
      printf("(MatSolve) done\n")
    }
  }
}
