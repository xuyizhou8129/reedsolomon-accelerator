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
  override lazy val module = new RoccAccImp(this)(p)
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
class RoccAccImp(outer: RoccAcc)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) with HasCoreParameters {
  // io is "implicit" because we inherit from LazyRoCCModuleImp.
  // io is the RoCCCoreIO
  val rocc_io = io
  val cmd = rocc_io.cmd
  val roccCmd = Reg(new RoCCCommand)
  val cmdValid = RegInit(false.B)

  val roccInst = roccCmd.inst // The customX instruction in instruction stream
  val returnReg = roccInst.rd
  val cmdStatus = roccCmd.status
  
  // Accept a new command only when the previous one has fully finished,
  // otherwise it would overwrite roccCmd mid-operation. Defined below.
  //TODO: should my accelerator have back presssure?
  val accel_busy = Wire(Bool())
  cmd.ready := !accel_busy
  
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

  // Which unit the current command targets. roccCmd is a register, so these
  // hold for the whole command.
  val is_alu = ExecUnit.UNIT_ALU === ctrl_sigs.unit
  val is_rs  = ExecUnit.UNIT_RS_ENC === ctrl_sigs.unit || ExecUnit.UNIT_RS_DEC === ctrl_sigs.unit

  // Remember when we need to respond (since cmd.valid is only high for one cycle)
  //sticky high for valid signal
  val response_needed = RegInit(false.B)

  // One-cycle pulse when a legal command has been captured.
  val cmd_start = cmdValid && ctrl_sigs.legal
  when(cmd_start) {
    cmdValid := false.B
  }

  /***************
   * DATA FETCH FOR ALU
   * Most instructions pass pointers to vectors, so we need to fetch that before
   * operating on the data.
   **************/
  val data_fetcher = Module(new DCacheFetcher)
  
  // Memory interface is shared, see MEMORY PORT below
  data_fetcher.io.resp := io.mem.resp
  
  // Control signals
  data_fetcher.io.start := cmd_start && is_alu
  data_fetcher.io.addr1 := roccCmd.rs1
  data_fetcher.io.addr2 := roccCmd.rs2

  /***************
   * DATA FETCH FOR ENCODER/DECODER
   * Instruction contract for RS_ENCODE and RS_DECODE:
   *   rs1 = address of the input symbols, one symbol per byte, xLen/8 aligned.
   *         Encode reads k symbols (the message), decode reads n (the received codeword).
   *   rs2 = address of the output buffer, xLen/8 aligned, n bytes are written.
   *         Encode writes the codeword, decode writes the error vector.
   *   rd  = status, bit 0 = corrupted, bit 1 = solved. Always 0 for encode.
   **************/
  val rs_n        = RS_Encoder.DEFAULT_N
  val rs_k        = RS_Encoder.DEFAULT_K
  //bits per symbol, equivalent to field size
  val rs_symBits  = GFOperations.DEFAULT_FIELD_SIZE
  // Bytes each symbol takes in memory
  val rs_symBytes = rs_symBits / 8 
  require(rs_symBits % 8 == 0, "symbol container must be byte-aligned")
  // How many symbols can fit in a single xLen-bit word
  val rs_symsPerWord = xLen / (8 * rs_symBytes)
  require(xLen % (8 * rs_symBytes) == 0, "symbol container must evenly divide xLen")
  val rs_maxWords = (rs_n + rs_symsPerWord - 1) / rs_symsPerWord //How many words are needed to hold n symbols, rounded up
  val rs_encWords = (rs_k + rs_symsPerWord - 1) / rs_symsPerWord //How many words are needed to hold k symbols, rounded up

  val rs_data_fetcher = Module(new BlockFetcher(rs_maxWords))
  val rs_data_writer  = Module(new BlockWriter(rs_maxWords))
  rs_data_fetcher.io.resp := io.mem.resp
  rs_data_writer.io.resp  := io.mem.resp

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
   * EXECUTE RS_Unit
   * fetch rs1 -> RSUnit -> write rs2 -> respond
   **************/
  val rs_unit = Module(new RSUnit)

  object RSState extends ChiselEnum {
    val sIdle,
        sFetch,   // BlockFetcher loading the input symbols
        sCompute, // RSUnit running
        sWrite,   // BlockWriter storing the output symbols
        sDone     // results in memory, respond if the instruction has rd
        = Value
  }
  val rs_state = RegInit(RSState.sIdle)

  // Unpack the fetched words into symbols, lowest byte first (little endian),
  // so symbol j is byte j of the input buffer.
  val rs_fetched_syms = rs_data_fetcher.io.data.flatMap { w =>
    (0 until rs_symsPerWord).map(i => w(8 * rs_symBytes * (i + 1) - 1, 8 * rs_symBytes * i))
  }
  val rs_is_decode = ExecUnit.UNIT_RS_DEC === ctrl_sigs.unit
  for (j <- 0 until rs_n) {
    // Encode only uses the first k symbols. Zero the rest so the bytes past
    // the message buffer never reach the unit.
    val sym = rs_fetched_syms(j)(rs_symBits - 1, 0)
    rs_unit.io.in(j) := (if (j < rs_k) sym else Mux(rs_is_decode, sym, 0.U))
  }
  rs_unit.io.op    := Mux(rs_is_decode, RSUnit.OP_DECODE, RSUnit.OP_ENCODE)
  rs_unit.io.start := false.B

  // Pack the output symbols back into words, same layout as the input.
  val rs_out_syms = rs_unit.io.out.map(_.pad(8 * rs_symBytes)) ++
    Seq.fill(rs_maxWords * rs_symsPerWord - rs_n)(0.U((8 * rs_symBytes).W))
  rs_data_writer.io.data := VecInit(rs_out_syms.grouped(rs_symsPerWord).map(g => Cat(g.reverse)).toSeq)

  rs_data_fetcher.io.start := false.B
  rs_data_fetcher.io.base  := roccCmd.rs1
  rs_data_fetcher.io.words := Mux(rs_is_decode, rs_maxWords.U, rs_encWords.U)
  rs_data_writer.io.start  := false.B
  rs_data_writer.io.base   := roccCmd.rs2
  rs_data_writer.io.bytes  := (rs_n * rs_symBytes).U

  // corrupted/solved are held by RSUnit until its next start
  val rs_status = Cat(rs_unit.io.solved, rs_unit.io.corrupted)

  switch(rs_state) {
    is(RSState.sIdle) {
      when(cmd_start && is_rs) {
        rs_data_fetcher.io.start := true.B
        rs_state := RSState.sFetch
      }
    }
    is(RSState.sFetch) {
      // done is held after the fetch completes, and was cleared by our start
      when(rs_data_fetcher.io.done) {
        rs_unit.io.start := true.B
        rs_state := RSState.sCompute
      }
    }
    is(RSState.sCompute) {
      when(rs_unit.io.done) {
        rs_data_writer.io.start := true.B
        rs_state := RSState.sWrite
      }
    }
    is(RSState.sWrite) {
      when(rs_data_writer.io.done) {
        rs_state := RSState.sDone
      }
    }
    is(RSState.sDone) {
      // Leave only once the response has been handed off below
      when(!response_needed) {
        rs_state := RSState.sIdle
      }
    }
  }

  /***************
   * MEMORY PORT
   * Only one requester is active at a time, so steer by which one is running.
   * Responses are broadcast and each unit only listens while it is active.
   **************/
  io.mem.req <> data_fetcher.io.req
  rs_data_fetcher.io.req.ready := false.B
  rs_data_writer.io.req.ready  := false.B
  when(rs_state === RSState.sFetch) {
    io.mem.req <> rs_data_fetcher.io.req
    data_fetcher.io.req.ready := false.B
  } .elsewhen(rs_state === RSState.sWrite) {
    io.mem.req <> rs_data_writer.io.req
    data_fetcher.io.req.ready := false.B
  }

  /***************
   * RESPOND
   **************/
  // Set response_needed when we get a valid command that needs a response
  when(cmd_start && roccInst.xd) {
    response_needed := true.B
  }
  
  val response = Reg(new RoCCResponse)
  val response_valid = RegInit(false.B)
  val response_fed = RegInit(false.B)
  
  // Default values
  io.resp.bits := response
  io.resp.valid := response_valid
  
  // Prepare response data when computation is complete
  when(is_alu && alu.io.valid && response_needed) { 
    response.data := alu_out
    response.rd := roccInst.rd
    response_fed := true.B
  }
  // RS results are already in memory, rd only carries the status
  when(is_rs && rs_state === RSState.sDone && response_needed && !response_fed) {
    response.data := rs_status
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

  // Busy from command capture until the response (if any) is handed off.
  // The core waits on io.busy for fences.
  // An illegal command keeps cmdValid high to hold the interrupt, but must
  // not block the next command, so only legal ones count.
  accel_busy := cmd_start || response_needed || response_valid ||
                rs_state =/= RSState.sIdle || data_fetcher.io.busy
  io.busy := accel_busy
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