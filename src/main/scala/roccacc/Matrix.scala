package roccacc

import chisel3._
import chisel3.util._

// Scratchpad memory for the RS decoder pipeline.
// Implements the same single-word port protocol used by MatSolve and CheckRoots:
// memReady is always asserted; read data is available combinationally in the same
// cycle as memRead (combinational read, registered write).
class Matrix(size: Int, addrWidth: Int = 32, dataWidth: Int = 8) extends Module {
  val io = IO(new Bundle {
    val memAddr  = Input(UInt(addrWidth.W))
    val memWData = Input(UInt(dataWidth.W))
    val memRData = Output(UInt(dataWidth.W))
    val memRead  = Input(Bool())
    val memWrite = Input(Bool())
    val memReady = Output(Bool())
  })

  val storage = RegInit(VecInit(Seq.fill(size)(0.U(dataWidth.W))))

  io.memReady := true.B
  io.memRData := Mux(io.memRead, storage(io.memAddr), 0.U)
  when(io.memWrite) {
    storage(io.memAddr) := io.memWData
  }
}
