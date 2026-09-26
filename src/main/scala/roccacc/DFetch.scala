package roccacc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile.CoreModule
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.rocket.HellaCacheIO
import freechips.rocketchip.rocket.HellaCacheReq
import freechips.rocketchip.rocket.HellaCacheResp

/* TODO: Investigate if we should use RoCCCoreIO.mem (DCacheFetcher) or
 * LazyRoCC.tlNode (DMemFetcher).
 * RoCCCoreIO.mem connects to the local main processor's L1 D$ to perform
 * operations.
 * LazyRoCC.tlNode connects to the L1-L2 crossbar connecting this tile to the
 * larger system. */

/** Module connecting VCode accelerator to main processor's non-blocking L1 data
  * cache.
  * @param p Implicit parameter passed by build system of top-level design parameters.
  */
class DCacheFetcher(implicit p: Parameters) extends CoreModule()(p) with MemoryOpConstants {
  /* For now, we only support "raw" loading and storing.
   * Only using M_XRD and M_XWR */
  
  val io = IO(new Bundle {
    // Control interface
    val start = Input(Bool())                    // Start fetching
    val addr1 = Input(UInt(xLen.W))             // First address to fetch
    val addr2 = Input(UInt(xLen.W))             // Second address to fetch
    val busy = Output(Bool())                    // Currently fetching
    val done = Output(Bool())                    // Fetching complete
    
    // Data output
    val data1 = Output(UInt(xLen.W))            // First fetched data
    val data2 = Output(UInt(xLen.W))            // Second fetched data
    val data1_valid = Output(Bool())            // First data valid
    val data2_valid = Output(Bool())            // Second data valid
    
    // Memory interface
    val req = Decoupled(new HellaCacheReq)
    val resp = Flipped(Valid(new HellaCacheResp))
  })
  
  
  // State machine - simplified like working version
  val idle :: fetching :: Nil = Enum(2) //Change this ( a linked list) proper chisel enum class to be used, can assign the encoding
  val state = RegInit(idle)
  val amount_fetched = RegInit(0.U(2.W))
  
  // Data registers
  val fetched_data1 = Reg(UInt(xLen.W))
  val fetched_data2 = Reg(UInt(xLen.W))
  val data1_valid_reg = RegInit(false.B)
  val data2_valid_reg = RegInit(false.B)
  
  // Done signal register
  val done_reg = RegInit(false.B)
  
  // Latch addresses to prevent them from changing during fetch
  val latched_addr1 = Reg(UInt(xLen.W))
  val latched_addr2 = Reg(UInt(xLen.W))
  
  // Default memory request values
  io.req.valid := false.B
  io.req.bits.addr := 0.U
  io.req.bits.cmd := M_XRD
  io.req.bits.size := 2.U  // 32-bit reads
  io.req.bits.signed := false.B
  io.req.bits.dprv := 0.U
  io.req.bits.dv := false.B
  io.req.bits.phys := false.B
  io.req.bits.no_resp := false.B
  io.req.bits.no_alloc := false.B
  io.req.bits.no_xcpt := false.B
  io.req.bits.data := 0.U
  io.req.bits.mask := 0.U
  io.req.bits.tag := 0.U
  
  // Output assignments
  io.busy := state =/= idle
  io.done := done_reg
  io.data1 := fetched_data1
  io.data2 := fetched_data2
  io.data1_valid := data1_valid_reg
  io.data2_valid := data2_valid_reg
  
  // State machine - simplified like working version
  switch(state) {
    is(idle) {
      amount_fetched := 0.U
      when(io.start) {
        // Latch addresses when starting
        latched_addr1 := io.addr1
        latched_addr2 := io.addr2
        //Reset data valid flags
        data1_valid_reg := false.B
        data2_valid_reg := false.B
        //Reset done signal
        done_reg := false.B
        state := fetching
      }
    }
    is(fetching) {
      when(amount_fetched === 0.U) {
        // First fetch
        io.req.valid := true.B
        io.req.bits.addr := latched_addr1 //Use latched address
        io.req.bits.tag := 1.U
      } .elsewhen(amount_fetched === 1.U) {
        // Second fetch
        io.req.valid := true.B
        io.req.bits.addr := latched_addr2 //Use latched address
        io.req.bits.tag := 2.U
      }
      
      when(io.resp.valid) {
        when(io.resp.bits.tag === 1.U && !data1_valid_reg) {
          // Only process if we haven't already received this data
          fetched_data1 := io.resp.bits.data
          data1_valid_reg := true.B
          amount_fetched := amount_fetched + 1.U
        } .elsewhen(io.resp.bits.tag === 2.U && !data2_valid_reg) {
          // Only process if we haven't already received this data
          fetched_data2 := io.resp.bits.data
          data2_valid_reg := true.B
          amount_fetched := amount_fetched + 1.U
        }
      }
      
      when(amount_fetched === 2.U) { //The cache system may repeat for example tag 2 response multiple times, need to consider being able to handle: increment amount fetched by one for each unique response tag I get back
        done_reg := true.B
        state := idle
      }
    }
  }
}

//Encoder and Decoder needs data fetching from memory that goes in blocks
class BlockFetcher(maxWords: Int)(implicit p: Parameters) extends CoreModule()(p) with MemoryOpConstants {
  val io = IO(new Bundle {
    val start = Input(Bool()) //once cycle pulse to start fetching
    val base  = Input(UInt(xLen.W)) //base adress of the first word
    val words = Input(UInt(log2Ceil(maxWords + 1).W)) //number of words to fetch, 0 to maxWords
    val data  = Output(Vec(maxWords, UInt(xLen.W))) //fetched data
    val valid = Output(Vec(maxWords, Bool())) //valid bits for each word
    val done  = Output(Bool()) //done signal
    val req   = Decoupled(new HellaCacheReq) //Interface to memory system, request channel
    val resp  = Flipped(Valid(new HellaCacheResp)) //Interface to memory system, response channel
  })
  // issue: for i in 0 until words, req.addr := base + i*(xLen/8), req.tag := i, size := log2Ceil(xLen/8)
  // collect: on resp.valid, data(resp.bits.tag) := resp.bits.data, mark bit i received
  // done := all `words` bits received

    // Each outstanding word is identified by its index in the tag field,
  // so the tag must be wide enough to hold maxWords - 1.
  require(maxWords <= (1 << dcacheReqTagBits), "maxWords exceeds cache tag space")

  val bytesPerWord = xLen / 8

  object State extends ChiselEnum { val idle, fetching = Value }
  val state = RegInit(State.idle)

  // Latched command
  val latched_base  = Reg(UInt(xLen.W))
  val latched_words = Reg(UInt(log2Ceil(maxWords + 1).W))

  // Progress tracking
  val issued   = RegInit(0.U(log2Ceil(maxWords + 1).W))  // how many requests sent
  val received = RegInit(0.U(maxWords.W))                // bitmask of words that arrived
  val done_reg = RegInit(false.B)

  // Data landing area
  val data_regs = Reg(Vec(maxWords, UInt(xLen.W)))

  // Mask with a 1 for every word we expect back
  val expected = ((1.U << latched_words) - 1.U)(maxWords - 1, 0)

  // Default request values (same as DCacheFetcher)
  io.req.valid         := false.B
  io.req.bits.addr     := 0.U
  io.req.bits.cmd      := M_XRD
  io.req.bits.size     := log2Ceil(bytesPerWord).U   // full xLen read, not 32-bit
  io.req.bits.signed   := false.B
  io.req.bits.dprv     := 0.U
  io.req.bits.dv       := false.B
  io.req.bits.phys     := false.B
  io.req.bits.no_resp  := false.B
  io.req.bits.no_alloc := false.B
  io.req.bits.no_xcpt  := false.B
  io.req.bits.data     := 0.U
  io.req.bits.mask     := 0.U
  io.req.bits.tag      := 0.U

  io.data := data_regs
  io.done := done_reg

  switch(state) {
    is(State.idle) {
      when(io.start) {
        latched_base  := io.base
        latched_words := io.words
        issued        := 0.U
        received      := 0.U
        done_reg      := false.B
        state         := State.fetching
      }
    }

    is(State.fetching) {
      // ISSUE: one request per cycle while the cache accepts them.
      // Unlike DCacheFetcher we do not wait for each response before
      // sending the next request, so several loads can be in flight.
      when(issued < latched_words) {
        io.req.valid     := true.B
        io.req.bits.addr := latched_base + (issued * bytesPerWord.U)
        io.req.bits.tag  := issued
      }
      //io.req.fire is a signal that is defined in Chisel that means AND of valid and ready
      when(io.req.fire) {
        issued := issued + 1.U
      }

      // COLLECT: responses may come back in any order, and the cache may
      // repeat one, so land each by tag and only count it the first time.
      when(io.resp.valid) {
        //truncate the tag to the number of bits needed
        val idx = io.resp.bits.tag(log2Ceil(maxWords) - 1, 0)
        when(!received(idx)) {
          data_regs(idx) := io.resp.bits.data
          //for each bit in received, if the index is equal to the tag, set that bit to 1
          received       := received | UIntToOH(idx, maxWords)
        }
      }

      // DONE: every expected word has landed.
      when(received === expected) {
        done_reg := true.B
        state    := State.idle
      }
    }
  }
}



/** Module connecting VCode accelerator directly to the L1-L2 crossbar connecting
  * all tiles to other components in the system.
  * @param p Implicit parameter passed by build system of top-level design parameters.
  */
class DMemFetcher(implicit p: Parameters) extends CoreModule()(p) {
  val io = IO(new Bundle {})
}