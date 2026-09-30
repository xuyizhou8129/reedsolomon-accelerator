package roccacc

import chisel3.util._

object Instructions {
  // To define custom instructions for Encoding and Decoding
  // Operand contract for both: see "DATA FETCH FOR ENCODER/DECODER" in Vcode.scala
  def RS_ENCODE = BitPat("b0000001") // rs1 = &message[k], rs2 = &codeword[n], rd = 0
  def RS_DECODE = BitPat("b0000010") // rs1 = &received[n], rs2 = &errorVec[n], rd = {solved, corrupted}
}