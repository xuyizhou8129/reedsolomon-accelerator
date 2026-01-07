package roccacc

import chisel3._
import chisel3.util._
import CustomVerilatorSim._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.ParallelTestExecution
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile.TileKey
import freechips.rocketchip.tile.TileParams
import org.chipsalliance.cde.config.Config
import freechips.rocketchip.rocket.RocketCoreParams

class RSEncoderSimpleTest extends AnyFunSpec with ParallelTestExecution {

  val fieldSize = 8
  val n = 15  // codeword length
  val k = 11  // message length

  // Create Parameters with minimal tile configuration for testing
  implicit val p: Parameters = new Config((site, here, up) => {
    case TileKey => new TileParams {
      val core = RocketCoreParams(nPMPs = 0)
      val icache = None
      val dcache = None
      val btb = None
      val tileId = 0
      val blockerCtrlAddr = None
      val baseName = "test_tile"
      val clockSinkParams = freechips.rocketchip.prci.ClockSinkParameters()
      val uniqueName = "test_tile_0"
    }
  })
// describe("RS Decoder Module Extension") {
//     it("extends message correctly") {
//       simulate(new RS_extender(fieldSize, n, k), buildDir = "build", enableWaves = true, testName = Some("extends message")) { dut =>
//         // Create test message with k=11 coefficients (using regular Scala Seq, not VecInit)
//         val testMessage = Seq(
//           "b00000001".U,  // coefficient 0
//           "b00000010".U,  // coefficient 1
//           "b00000011".U,  // coefficient 2
//           "b00000100".U,  // coefficient 3
//           "b00000101".U,  // coefficient 4
//           "b00000110".U,  // coefficient 5
//           "b00000111".U,  // coefficient 6
//           "b00001000".U,  // coefficient 7
//           "b00001001".U,  // coefficient 8
//           "b00001010".U,  // coefficient 9
//           "b00001011".U   // coefficient 10
//         )
        
  //       // Provide message input
  //       for (i <- 0 until k) {
  //         dut.io.message(i).poke(testMessage(i))
  //       }
  //       //dut.io.message.valid.poke(true.B)
  //       for (i <- 0 until k){
  //          dut.io.out(i).expect(testMessage(i))
  //       }
  //        for (i <- k until n){
  //          dut.io.out(i).expect(0.U)
  //       }

  //       // Wait for processing to complete (Int.MaxValue = effectively unlimited)
  //       //dut.clock.stepUntil(dut.io.out.valid, 1, Int.MaxValue)
        
  //       // Check output validity
  //       //dut.io.out.valid.expect(true.B)
  //     }
  //   }
  // }
  

  describe("HW Encoding tested against SWModel") {
    it("encodes RS(15,11) message and compares HW result with SW golden model") {
      // Create test message with k=11 coefficients
      val testMessage = (2 to 12).map(BigInt(_))
      
      // Step 1: Get software (golden) result
      val swCodeword = SWModel.encode(testMessage, n, k, fieldSize)
      
      // Verify software codeword length is n=15
      assert(swCodeword.length == n, s"Expected codeword length $n, got ${swCodeword.length}")
      
      // Print software results for debugging
      println(s"Message: ${testMessage.mkString(", ")}")
      println(s"SW Codeword: ${swCodeword.mkString(", ")}")
      
      // Step 2: Test hardware encoder
      simulate(new RS_Encoder(fieldSize, n, k), buildDir = "build", enableWaves = true, testName = Some("hw_vs_sw_encoding")) { dut =>
        // Convert test message to Chisel UInt format
        val testMessageUInt = testMessage.map(_.U(fieldSize.W))
        
        // Provide message input to hardware
        for (i <- 0 until k) {
          dut.io.message.bits(i).poke(testMessageUInt(i))
        }
        dut.io.message.valid.poke(true.B)
        
        // Wait for processing to complete
        dut.clock.stepUntil(dut.io.out.valid, 1, Int.MaxValue)
        
        // Check output validity
        dut.io.out.valid.expect(true.B)
        
        // Step 3: Compare hardware result with software (golden) result
        println(s"HW Codeword: ")
        for (i <- 0 until n) {
          val hwValue = dut.io.out.bits(i).peek().litValue
          val swValue = swCodeword(i)
          println(s"  [${i}]: HW=${hwValue}, SW=${swValue}")
          
          // Assert hardware result equals software result
          assert(hwValue == swValue, 
            s"Mismatch at index $i: HW=${hwValue}, SW=${swValue}")
        }
        
        println("Success: Hardware encoding matches software (golden) model!")
      }
    }
  }
}