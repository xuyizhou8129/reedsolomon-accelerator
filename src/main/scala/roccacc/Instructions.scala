package roccacc

import chisel3.util._

object Instructions {
  def PLUS_INT = BitPat("b0000000")
  // To define custom instructions for Encoding and Decoding
  def ENCODE = BitPat("b0000001")
  def DECODE = BitPat("b0000010")
}