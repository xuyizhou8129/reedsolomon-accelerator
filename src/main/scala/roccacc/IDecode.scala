package roccacc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile.HasCoreParameters
import Instructions._
import roccacc.constants._
import ExecUnit._

/** Which execution unit an instruction targets. Produced by the decode table
  * and consumed by RoccAccImp to steer the fetched operands. */
object ExecUnit {
  val SZ_UNIT = 2.W
  def UNIT_X      = BitPat("b??")
  def UNIT_RS_ENC = BitPat("b01")
  def UNIT_RS_DEC = BitPat("b10")
}

/** Trait holding an abstract (non-instantiated) mapping between the instruction
  * bit pattern and its control signals.
  */
trait DecodeConstants extends HasCoreParameters { // TODO: Not sure if extends needed
  /** Array of pairs (table) mapping between instruction bit patterns and control
    * signals. */
  val decode_table: Array[(BitPat, List[BitPat])]
}

/** Control signals in the processor.
  * These are set during decoding.
  */
class CtrlSigs extends Bundle {
  /* All control signals used in this coprocessor
   * See rocket-chip's rocket/IDecode.scala#IntCtrlSigs#default */
  val legal = Bool() // Whether the funct7 matched an entry in the decode table
  val unit = Bits(SZ_UNIT) // Which execution unit handles this instruction
  /** List of default control signal values
    * @return List of default control signal values. */
    //Anything that doesnt match the decode table will fall back to List(N, UNIT_X)
  def default_decode_ctrl_sigs: List[BitPat] =
    List(N, UNIT_X)

  /** Decodes an instruction to its control signals.
    * @param inst The instruction bit pattern to be decoded.
    * @param table Table of instruction bit patterns mapping to list of control
    * signal values.
    * @return Sequence of control signal values for the provided instruction.
    */
  def decode(inst: UInt, decode_table: Iterable[(BitPat, List[BitPat])]) = {
    //Currently the decoder is a generated hardware with 1-bit wire for legal and a 2-bit wire for unit
    //inst is the funct field of the instruction, which is 7 bits wide.
    val decoder = freechips.rocketchip.rocket.DecodeLogic(inst, default_decode_ctrl_sigs, decode_table)
    /* Make sequence ordered how signals are ordered.
     * See rocket-chip's rocket/IDecode.scala#IntCtrlSigs#decode#sigs */
    val ctrl_sigs = Seq(legal, unit)
    /* Decoder is a minimized truth-table. We partially apply the map here,
     * which allows us to apply an instruction to get its control signals back.
     * We then zip that with the sequence of names for the control signals. */
    ctrl_sigs zip decoder map{case(s,d) => s := d}
    this
  }
}

/** Class holding a table that implements the DecodeConstants table that mapping
  * a binary operation's instruction bit pattern to control signals.
  * @param p Implicit parameter of key-value pairs that can globally alter the
  * parameters of the design during elaboration.
  */
class BinOpDecode(implicit val p: Parameters) extends DecodeConstants {
  val decode_table: Array[(BitPat, List[BitPat])] = Array(
    RS_ENCODE -> List(Y, UNIT_RS_ENC),
    RS_DECODE -> List(Y, UNIT_RS_DEC)
  )
}