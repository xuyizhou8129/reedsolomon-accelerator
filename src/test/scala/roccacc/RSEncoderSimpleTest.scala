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
  describe("RS Decoder Module") {
    it("decodes RS(15,11) message") {
      simulate(new RS_Decoder(fieldSize, n, k), buildDir = "build", enableWaves = true, testName = Some("decodes_RS_15_11_message")) { dut =>
        // Create test message with k=11 coefficients (using regular Scala Seq, not VecInit)
        val testMessage = Seq(
          "b00000001".U,  // coefficient 0
          "b00000010".U,  // coefficient 1
          "b00000011".U,  // coefficient 2
          "b00000100".U,  // coefficient 3
          "b00000101".U,  // coefficient 4
          "b00000110".U,  // coefficient 5
          "b00000111".U,  // coefficient 6
          "b00001000".U,  // coefficient 7
          "b00001001".U,  // coefficient 8
          "b00001010".U,  // coefficient 9
          "b00001011".U   // coefficient 10
        )
        
        // Provide message input
        for (i <- 0 until k) {
          dut.io.message.bits(i).poke(testMessage(i))
        }
        dut.io.message.valid.poke(true.B)
        
        // Wait for processing to complete (Int.MaxValue = effectively unlimited)
        dut.clock.stepUntil(dut.io.out.valid, 1, Int.MaxValue)
        
        // Check output validity
        dut.io.out.valid.expect(true.B)
      }
    }
  }
}