package roccacc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile.CoreModule

//The MatSolve module conducts Ax=b by Gaussian elimination.
//uses a central scratchpad RAM: one memory block stores the matrix data (Matrix.scala)
//and my MatSolve module computes read from it and write results back to it
//local buffers for rows and columns can be used to store data
//Keep the current header comments and example IO interface
//The current implementation still uses a local buffer for everything
//use the current code as a template for the new implementation
//use state machines and initiate hardware modules from GFOperations.scala
//do not modify GFOperations.scala

class MatSolveIO(addrWidth: Int, dataWidth: Int) extends Bundle {
  val start = Input(Bool())
  val done = Output(Bool())
  val busy = Output(Bool())
  val unsolvable = Output(Bool())

  val baseA = Input(UInt(addrWidth.W))
  val baseB = Input(UInt(addrWidth.W))
  val baseX = Input(UInt(addrWidth.W))

  val memAddr = Output(UInt(addrWidth.W))
  val memWData = Output(UInt(dataWidth.W))
  val memRData = Input(UInt(dataWidth.W))
  val memRead = Output(Bool())
  val memWrite = Output(Bool())
  val memReady = Input(Bool())
}

class MatSolve(rows: Int, cols: Int, addrWidth: Int = 32) extends Module {
  val fs = GFOperations.DEFAULT_FIELD_SIZE
  require(fs == 8, "MatSolve dataWidth tied to GF(2^8) elements in memory")
  val ww = 2 * fs

  val io = IO(new MatSolveIO(addrWidth, fs))

  val gfMul = Module(new GFMul(fs))
  val gfAdd = Module(new GFAdd(fs))
  val gfDiv = Module(new GFDiv(fs))

  gfMul.io.in1.bits := 0.U(ww.W)
  gfMul.io.in1.valid := false.B
  gfMul.io.in2.bits := 0.U(ww.W)
  gfMul.io.in2.valid := false.B
  gfAdd.io.in1.bits := 0.U(ww.W)
  gfAdd.io.in1.valid := false.B
  gfAdd.io.in2.bits := 0.U(ww.W)
  gfAdd.io.in2.valid := false.B
  gfDiv.io.in1.bits := 0.U(ww.W)
  gfDiv.io.in1.valid := false.B
  gfDiv.io.in2.bits := 0.U(ww.W)
  gfDiv.io.in2.valid := false.B

  object MatSolveState extends ChiselEnum {
    val idle, loadA, loadB, findpivot, eliminate, backsubstitution, storeX, donePulse = Value
  }

  object Wp extends ChiselEnum {
    val fwFind, fwInvS, fwInvW, fwNormS, fwNormW, fwElimEnter, fwElimMulS, fwElimMulW, fwAddS, fwAddW, fwNextCol, bsRow, bsMulS, bsMulW, bsAddS, bsAddW = Value
  }

  val MatSolve_state = RegInit(MatSolveState.idle)
  val wp = RegInit(Wp.fwFind)

  val baseAReg = Reg(UInt(addrWidth.W))
  val baseBReg = Reg(UInt(addrWidth.W))
  val baseXReg = Reg(UInt(addrWidth.W))

  val loadRow = Reg(UInt(log2Ceil(math.max(rows, 1)).W))
  val loadCol = Reg(UInt(log2Ceil(math.max(cols, 1)).W))
  val loadBRow = Reg(UInt(log2Ceil(math.max(rows, 1)).W))
  val storeXIdx = Reg(UInt(log2Ceil(math.max(cols, 1)).W))

  val aug = Reg(Vec(rows, Vec(cols + 1, UInt(fs.W))))
  val numPivots = Reg(UInt(log2Ceil(rows + 1).W))
  val colIdx = Reg(UInt(log2Ceil(cols + 1).W))

  val hasPivot = Reg(Bool())
  val invP = Reg(UInt(fs.W))
  val jNorm = Reg(UInt(log2Ceil(cols + 2).W))
  val elimR = Reg(UInt(log2Ceil(rows).W))
  val elimJ = Reg(UInt(log2Ceil(cols + 2).W))
  val fac = Reg(UInt(fs.W))
  val prod = Reg(UInt(fs.W))

  val pivCol = Reg(Vec(rows, UInt(log2Ceil(math.max(cols, 1)).W)))
  val bsI = Reg(UInt(log2Ceil(rows + 1).W))
  val bsJ = Reg(UInt(log2Ceil(rows + 1).W))
  val bsAcc = Reg(UInt(fs.W))

  val xReg = Reg(Vec(cols, UInt(fs.W)))
  val unsolv = RegInit(false.B)

  // Default memory interface
  io.memAddr := 0.U(addrWidth.W)
  io.memWData := 0.U(fs.W)
  io.memRead := false.B
  io.memWrite := false.B
  io.done := false.B
  io.busy := MatSolve_state =/= MatSolveState.idle
  io.unsolvable := unsolv

  def pz(u: UInt): UInt = u.pad(ww)

  switch(MatSolve_state) {
    is(MatSolveState.idle) {
      unsolv := false.B
      when(io.start) {
        baseAReg := io.baseA
        baseBReg := io.baseB
        baseXReg := io.baseX
        loadRow := 0.U
        loadCol := 0.U
        MatSolve_state := MatSolveState.loadA
        printf("(MatSolve) start: load from memory\n")
      }
    }

    is(MatSolveState.loadA) {
      val aOff = (loadRow * cols.U) + loadCol
      io.memAddr := baseAReg + aOff
      io.memRead := true.B
      when(io.memReady && io.memRead) {
        aug(loadRow)(loadCol) := io.memRData
        when(loadRow === (rows - 1).U && loadCol === (cols - 1).U) {
          loadBRow := 0.U
          MatSolve_state := MatSolveState.loadB
        }.elsewhen(loadCol === (cols - 1).U) {
          loadCol := 0.U
          loadRow := loadRow + 1.U
        }.otherwise {
          loadCol := loadCol + 1.U
        }
      }
    }

    is(MatSolveState.loadB) {
      io.memAddr := baseBReg + loadBRow
      io.memRead := true.B
      when(io.memReady && io.memRead) {
        aug(loadBRow)(cols) := io.memRData
        when(loadBRow === (rows - 1).U) {
          numPivots := 0.U
          colIdx := 0.U
          wp := Wp.fwFind
          MatSolve_state := MatSolveState.findpivot
        }.otherwise {
          loadBRow := loadBRow + 1.U
        }
      }
    }

    is(MatSolveState.findpivot) {
      switch(wp) {
        is(Wp.fwFind) {
          val rel = Wire(Vec(rows, Bool()))
          for (r <- 0 until rows) {
            rel(r) := (r.U >= numPivots) && aug(r)(colIdx) =/= 0.U
          }
          val hp = numPivots < rows.U && rel.asUInt.orR
          val pr = numPivots + PriorityEncoder(rel.asUInt)
          hasPivot := hp
          when(hp) {
            for (j <- 0 to cols) {
              val a = aug(numPivots)(j)
              val b = aug(pr)(j)
              aug(numPivots)(j) := Mux(pr =/= numPivots, b, a)
              aug(pr)(j) := Mux(pr =/= numPivots, a, b)
            }
            wp := Wp.fwInvS
          }.otherwise {
            wp := Wp.fwNextCol
          }
        }

        is(Wp.fwInvS) {
          when(gfDiv.io.in1.ready && gfDiv.io.in2.ready) {
            gfDiv.io.in1.bits := 1.U(ww.W)
            gfDiv.io.in1.valid := true.B
            gfDiv.io.in2.bits := pz(aug(numPivots)(colIdx))
            gfDiv.io.in2.valid := true.B
            wp := Wp.fwInvW
          }
        }

        is(Wp.fwInvW) {
          when(gfDiv.io.out.valid) {
            invP := gfDiv.io.out.bits
            gfDiv.io.in1.valid := false.B
            gfDiv.io.in2.valid := false.B
            jNorm := colIdx
            wp := Wp.fwNormS
          }
        }

        is(Wp.fwNormS) {
          when(gfMul.io.in1.ready && gfMul.io.in2.ready) {
            gfMul.io.in1.bits := pz(aug(numPivots)(jNorm))
            gfMul.io.in1.valid := true.B
            gfMul.io.in2.bits := pz(invP)
            gfMul.io.in2.valid := true.B
            wp := Wp.fwNormW
          }
        }

        is(Wp.fwNormW) {
          when(gfMul.io.out.valid) {
            aug(numPivots)(jNorm) := gfMul.io.out.bits
            gfMul.io.in1.valid := false.B
            gfMul.io.in2.valid := false.B
            when(jNorm === cols.U) {
              elimR := numPivots + 1.U
              wp := Wp.fwElimEnter
            }.otherwise {
              jNorm := jNorm + 1.U
              wp := Wp.fwNormS
            }
          }
        }

        is(Wp.fwElimEnter) {
          when(elimR >= rows.U) {
            wp := Wp.fwNextCol
          }.elsewhen(aug(elimR)(colIdx) === 0.U) {
            elimR := elimR + 1.U
          }.otherwise {
            fac := aug(elimR)(colIdx)
            elimJ := colIdx
            wp := Wp.fwElimMulS
          }
        }

        is(Wp.fwElimMulS) {
          when(gfMul.io.in1.ready && gfMul.io.in2.ready) {
            gfMul.io.in1.bits := pz(fac)
            gfMul.io.in1.valid := true.B
            gfMul.io.in2.bits := pz(aug(numPivots)(elimJ))
            gfMul.io.in2.valid := true.B
            wp := Wp.fwElimMulW
          }
        }

        is(Wp.fwElimMulW) {
          when(gfMul.io.out.valid) {
            prod := gfMul.io.out.bits
            gfMul.io.in1.valid := false.B
            gfMul.io.in2.valid := false.B
            wp := Wp.fwAddS
          }
        }

        is(Wp.fwAddS) {
          when(gfAdd.io.in1.ready && gfAdd.io.in2.ready) {
            gfAdd.io.in1.bits := pz(aug(elimR)(elimJ))
            gfAdd.io.in1.valid := true.B
            gfAdd.io.in2.bits := pz(prod)
            gfAdd.io.in2.valid := true.B
            wp := Wp.fwAddW
          }
        }

        is(Wp.fwAddW) {
          when(gfAdd.io.out.valid) {
            aug(elimR)(elimJ) := gfAdd.io.out.bits
            gfAdd.io.in1.valid := false.B
            gfAdd.io.in2.valid := false.B
            when(elimJ === cols.U) {
              elimR := elimR + 1.U
              wp := Wp.fwElimEnter
            }.otherwise {
              elimJ := elimJ + 1.U
              wp := Wp.fwElimMulS
            }
          }
        }

        is(Wp.fwNextCol) {
          when(hasPivot) {
            numPivots := numPivots + 1.U
          }
          when(colIdx === (cols - 1).U) {
            MatSolve_state := MatSolveState.eliminate
          }.otherwise {
            colIdx := colIdx + 1.U
            wp := Wp.fwFind
          }
        }
      }
    }

    is(MatSolveState.eliminate) {
      val bad = (0 until rows).map { r =>
        val ge = r.U >= numPivots
        val az = (0 until cols).map(c => aug(r)(c) === 0.U).reduce(_ && _)
        ge && az && aug(r)(cols) =/= 0.U
      }.reduce(_ || _)
      unsolv := bad
      when(bad) {
        MatSolve_state := MatSolveState.donePulse
      }.elsewhen(numPivots === 0.U) {
        storeXIdx := 0.U
        MatSolve_state := MatSolveState.storeX
      }.otherwise {
        for (r <- 0 until rows) {
          val fl = VecInit((0 until cols).map(c => aug(r)(c) =/= 0.U))
          pivCol(r) := PriorityEncoder(fl.asUInt)
        }
        bsI := numPivots - 1.U
        wp := Wp.bsRow
        MatSolve_state := MatSolveState.backsubstitution
      }
      printf("(MatSolve) eliminate\n")
    }

    is(MatSolveState.backsubstitution) {
      switch(wp) {
        is(Wp.bsRow) {
          when(bsI >= numPivots) {
            storeXIdx := 0.U
            MatSolve_state := MatSolveState.storeX
          }.otherwise {
            val ci = pivCol(bsI)
            bsAcc := aug(bsI)(cols)
            bsJ := bsI + 1.U
            when(bsI + 1.U >= numPivots) {
              xReg(ci) := aug(bsI)(cols)
              when(bsI === 0.U) {
                storeXIdx := 0.U
                MatSolve_state := MatSolveState.storeX
              }.otherwise {
                bsI := bsI - 1.U
              }
            }.otherwise {
              wp := Wp.bsMulS
            }
          }
        }

        is(Wp.bsMulS) {
          when(gfMul.io.in1.ready && gfMul.io.in2.ready) {
            gfMul.io.in1.bits := pz(aug(bsI)(pivCol(bsJ)))
            gfMul.io.in1.valid := true.B
            gfMul.io.in2.bits := pz(xReg(pivCol(bsJ)))
            gfMul.io.in2.valid := true.B
            wp := Wp.bsMulW
          }
        }

        is(Wp.bsMulW) {
          when(gfMul.io.out.valid) {
            prod := gfMul.io.out.bits
            gfMul.io.in1.valid := false.B
            gfMul.io.in2.valid := false.B
            wp := Wp.bsAddS
          }
        }

        is(Wp.bsAddS) {
          when(gfAdd.io.in1.ready && gfAdd.io.in2.ready) {
            gfAdd.io.in1.bits := pz(bsAcc)
            gfAdd.io.in1.valid := true.B
            gfAdd.io.in2.bits := pz(prod)
            gfAdd.io.in2.valid := true.B
            wp := Wp.bsAddW
          }
        }

        is(Wp.bsAddW) {
          when(gfAdd.io.out.valid) {
            val newAcc = gfAdd.io.out.bits
            bsAcc := newAcc
            gfAdd.io.in1.valid := false.B
            gfAdd.io.in2.valid := false.B
            val nextJ = bsJ + 1.U
            bsJ := nextJ
            val ci = pivCol(bsI)
            when(nextJ >= numPivots) {
              xReg(ci) := newAcc
              when(bsI === 0.U) {
                storeXIdx := 0.U
                MatSolve_state := MatSolveState.storeX
              }.otherwise {
                bsI := bsI - 1.U
                wp := Wp.bsRow
              }
            }.otherwise {
              wp := Wp.bsMulS
            }
          }
        }
      }
    }

    is(MatSolveState.storeX) {
      io.memAddr := baseXReg + storeXIdx
      io.memWData := xReg(storeXIdx)
      io.memWrite := true.B
      when(io.memReady && io.memWrite) {
        when(storeXIdx === (cols - 1).U) {
          MatSolve_state := MatSolveState.donePulse
        }.otherwise {
          storeXIdx := storeXIdx + 1.U
        }
      }
    }

    is(MatSolveState.donePulse) {
      io.done := true.B
      MatSolve_state := MatSolveState.idle
      printf("(MatSolve) done\n")
    }
  }
}
