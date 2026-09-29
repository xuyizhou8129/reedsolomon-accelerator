package roccacc

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.rocket.constants.MemoryOpConstants

/** The outer wrapping class for the RoccAcc accelerator.
  *
  * @constructor Create a new RoccAcc accelerator interface using one of the
  * custom opcode sets.
  * @param opcodes The custom opcode set to use.
  * @param p The implicit key-value store of design parameters for this design.
  * This value is passed by the build system. You do not need to worry about it.
  */
class RoccAcc(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new RoccAccImp(this)
}

/** Implementation class for the RoccAcc accelerator.
  *
  * @constructor Create a new RoccAcc accelerator implementation, attached to
  * one RoccAcc interface with one of the custom opcode sets.
  * @param outer The "interface" for the accelerator to attach to.
  * This separation allows us to attach multiple of these accelerators to
  * different HARTs, and multiple to attach to a single HART using different
  * custom opcode sets.
  */
class RoccAccImp(outer: RoccAcc) extends LazyRoCCModuleImp(outer) {
  // io is "implicit" because we inherit from LazyRoCCModuleImp.
  // io is the RoCCCoreIO
  val rocc_io = io
  val cmd = rocc_io.cmd
  val roccCmd = Reg(new RoCCCommand)
  val cmdValid = RegInit(false.B)

  val roccInst = roccCmd.inst // The customX instruction in instruction stream
  val returnReg = roccInst.rd
  val cmdStatus = roccCmd.status
  
  // Always ready to accept commands
  cmd.ready := true.B
  
  when(cmd.fire) {
    roccCmd := cmd.bits // The entire RoCC Command provided to the accelerator
    cmdValid := true.B
  }
  /* Create the decode table at the top-level of the implementation
   * If additional instructions are added as separate classes in Instructions.scala
   * they can be added above BinOpDecode class. */
  val decode_table = {
    Seq(new BinOpDecode)
  } flatMap(_.decode_table)

  /***************
   * DECODE
   **************/
  // Decode instruction, yielding control signals
  val ctrl_sigs = Wire(new CtrlSigs()).decode(roccInst.funct, decode_table)

  // If invalid instruction, raise exception
  val exception = cmdValid && !ctrl_sigs.legal
  io.interrupt := exception
  when(exception) {
    if(p(RoccAccPrintfEnable)) {
      printf("Raising exception to processor through interrupt!\nILLEGAL INSTRUCTION!\n");
    }
  }

  /***************
   * DATA FETCH FOR ALU
   * Most instructions pass pointers to vectors, so we need to fetch that before
   * operating on the data.
   **************/
  val data_fetcher = Module(new DCacheFetcher)
  
  // Connect memory interface using separated req/resp like working implementation
  data_fetcher.io.req <> io.mem.req
  data_fetcher.io.resp <> io.mem.resp
  
  // Control signals
  data_fetcher.io.start := cmdValid && ctrl_sigs.legal
  data_fetcher.io.addr1 := roccCmd.rs1
  data_fetcher.io.addr2 := roccCmd.rs2

    /***************
   * DATA FETCH FOR ENCODER/DECODER
   **************/
  val rs_data_fetcher = Module(new BlockFetcher)

   // Connect the block fetcher to the memory interface
  rs_data_fetcher.io.req <> io.mem.req
  rs_data_fetcher.io.resp <> io.mem.resp

   // Control signals
  rs_data_fetcher.io.start := cmdValid && ctrl_sigs.legal
  rs_data_fetcher.io.base := roccCmd.rs1
  rs_data_fetcher.io.words := roccCmd.rs2


  /***************
   * EXECUTE ALU
   **************/
  val alu = Module(new roccacc.ALU)
  val alu_out = Wire(UInt())
  // Hook up the ALU to RoccAcc signals
  alu.io.dw := 1.U(1.W)  // Use 64-bit operations for now
  alu.io.fn := Mux(data_fetcher.io.data1_valid && data_fetcher.io.data2_valid, ctrl_sigs.alu_fn, 1.U)
  // Use fetched data, otherwise the inputs are 0
  alu.io.in1 := Mux(data_fetcher.io.data1_valid, data_fetcher.io.data1, 0.U)
  alu.io.in2 := Mux(data_fetcher.io.data2_valid, data_fetcher.io.data2, 0.U)
  alu_out := alu.io.out

  /***************
   * EXECUTE ALU
   **************/
  // RS encode/decode datapath unit (see RSUnit.scala for the interface contract).
  // TODO: drive `in` from the fetched words (unpacked into symbols), `start`
  // from fetch-complete, and consume `out`/`done` in the RESPOND stage.
  // `ctrl_sigs.unit` (UNIT_RS_ENC / UNIT_RS_DEC) selects `op`.
  val rs_unit = Module(new RSUnit)
  rs_unit.io.op    := RSUnit.OP_ENCODE
  rs_unit.io.start := false.B
  rs_unit.io.in.foreach(_ := 0.U)

  /***************
   * RESPOND
   **************/
  // Remember when we need to respond (since cmd.valid is only high for one cycle)
  val response_needed = RegInit(false.B)
  
  // Set response_needed when we get a valid command that needs a response
  when(cmdValid && ctrl_sigs.legal && roccInst.xd) {
    response_needed := true.B
    cmdValid := false.B
  }
  
  val response = Reg(new RoCCResponse)
  val response_valid = RegInit(false.B)
  val response_fed = RegInit(false.B)
  
  // Default values
  io.resp.bits := response
  io.resp.valid := response_valid
  
  // Prepare response data when computation is complete
  when(alu.io.valid && response_needed) { 
    response.data := alu_out
    response.rd := roccInst.rd
    response_fed := true.B
  }
  //Fire when response is ready to be sent
  when(response_fed){
    response_valid := true.B
    response_needed := false.B
    response_fed := false.B
        if(p(RoccAccPrintfEnable)) {
      printf("Got funct7 = 0x%x\trs1.val=0x%x\trs2.val=0x%x\n", roccInst.funct, roccCmd.rs1, roccCmd.rs2)
      printf("The response is: %d\n", response.data)
    }
  }
  
  // Clear response valid when handshake occurs
  when(response_valid && io.resp.ready) {
    response_valid := false.B
  }
  }


/** Mixin to build a chip that includes a RoccAcc accelerator. */
class WithRoccAcc extends Config((site, here, up) => {
  case BuildRoCC => List (
    (p: Parameters) => {
      val roccAcc = LazyModule(new RoccAcc(OpcodeSet.custom0)(p))
      roccAcc
    })
})

/** Design-level configuration option to toggle the synthesis of print statements
  * in the synthesized hardware design.
  */
case object RoccAccPrintfEnable extends Field[Boolean](false)

/** Mixin to enable print statements from the synthesized design.
  * This mixin should only be used AFTER the WithRoccAcc mixin.
  */
class WithRoccAccPrintf extends Config((site, here, up) => {
  case RoccAccPrintfEnable => true
})