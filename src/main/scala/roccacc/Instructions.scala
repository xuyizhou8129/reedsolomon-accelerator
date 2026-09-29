package roccacc

import chisel3.util._

object Instructions {
  def PLUS_INT = BitPat("b0000000")
  // To define custom instructions for Encoding and Decoding
  def RS_ENCODE = BitPat("b0000001")
  def RS_DECODE = BitPat("b0000010")
}