package roccacc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile.CoreModule

//Creating a matrix module to store the matrix in the register
//The matrix is stored in a vector of vectors of UInts
//There is a one cycle delay between the input and output
//No handshake protocol is used, the input is a matrix and the output is a matrix
class Matrix(rows: Int, cols: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(rows, Vec(cols, UInt(8.W)))) // Input matrix is a vector of vectors of UInts
    val out = Output(Vec(rows, Vec(cols, UInt(8.W)))) // Output matrix is a vector of vectors of UInts
  })
  
  val matrix = RegInit(VecInit(Seq.fill(rows)(VecInit(Seq.fill(cols)(0.U(8.W))))))
  
  for (i <- 0 until rows) {
    for (j <- 0 until cols) {
      matrix(i)(j) := io.in(i)(j) //copy the input matrix to the matrix register
    }
  }
  
  io.out := matrix //The output is a matrix
}