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

class GFOperationsSimpleTest extends AnyFunSpec with ParallelTestExecution {

  val fieldSize = 8

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

  describe("GFOperations Module") {

    it("performs GF Reduction") {
      simulate(new GFReduce(fieldSize), buildDir = "build", enableWaves = true) { dut =>
      // Reduction of 100011101
        dut.io.in1.valid.poke(true.B)              
        dut.io.in1.bits.poke("b100011101".U) 
        while (!dut.io.out.valid.peek().litToBoolean) {
    dut.clock.step()
  }
        dut.io.out.bits.expect("b00000000".U)
        dut.io.out.valid.expect(true.B)

    // Reset for second test
    dut.io.in1.valid.poke(false.B)  // Reset valid signal
    dut.clock.step(2)  // Wait for module to return to idle and stabilize

        // Reduction of 1100011101
        dut.io.in1.valid.poke(true.B)             
        dut.io.in1.bits.poke("b1100011101".U)  
        while (!dut.io.out.valid.peek().litToBoolean) {
    dut.clock.step()
  }
        dut.io.out.bits.expect("b00111010".U)
        dut.io.out.valid.expect(true.B)
      }
    }

    it("performs GF Addition") {
      simulate(new GFAdd(fieldSize), buildDir = "build", enableWaves = true) { dut =>
        // Addition of 100011101 and 100011101
        dut.io.in1.valid.poke(true.B)
        dut.io.in2.valid.poke(true.B)
        dut.io.in1.bits.poke("b100011101".U)
        dut.io.in2.bits.poke("b100011101".U) 
       while (!dut.io.out.valid.peek().litToBoolean) {
    dut.clock.step()
  }
        dut.io.out.bits.expect("b00000000".U)
        dut.io.out.valid.expect(true.B)

        // Reset for second test
        dut.io.in1.valid.poke(false.B)  // Reset valid signal
        dut.clock.step(3)  // Wait for module to return to idle and stabilize

       // Addition of 1100011101 and 100011101
        dut.io.in1.valid.poke(true.B)             
        dut.io.in1.bits.poke("b1100011101".U)  
        dut.io.in2.valid.poke(true.B)
        dut.io.in2.bits.poke("b100011101".U)
        while (!dut.io.out.valid.peek().litToBoolean) {
    dut.clock.step()
  }
        dut.io.out.bits.expect("b00111010".U)
        dut.io.out.valid.expect(true.B)

         dut.io.in1.valid.poke(false.B)  // Reset valid signal
         dut.io.in2.valid.poke(false.B)  // Reset valid signal
        dut.clock.step(3)  // Wait for module to return to idle and stabilize

        // Addition of 1100101101 and 1100011101
        dut.io.in1.valid.poke(true.B)             
        dut.io.in1.bits.poke("b1100101101".U)  
        dut.io.in2.valid.poke(true.B)
        dut.io.in2.bits.poke("b1100011101".U)
        while (!dut.io.out.valid.peek().litToBoolean) {
    dut.clock.step()
  }
        dut.io.out.bits.expect("b00110000".U)
        dut.io.out.valid.expect(true.B)
      }
    }

     it("performs GF Multiplication") {
      simulate(new GFMul(fieldSize), buildDir = "build", enableWaves = true) { dut =>
        // Multiplication of 1100011101 and 1001011101
        dut.io.in1.valid.poke(true.B)
        dut.io.in2.valid.poke(true.B)
        dut.io.in1.bits.poke("b1100011101".U)
        dut.io.in2.bits.poke("b1001011101".U) 
        dut.clock.stepUntil(dut.io.out.valid, 1, 50)

        dut.io.out.bits.expect("b10010011".U)
        dut.io.out.valid.expect(true.B)

        dut.io.in1.valid.poke(false.B)  // Reset valid signal
        dut.io.in2.valid.poke(false.B)  // Reset valid signal
        dut.clock.step(3)  // Wait for module to return to idle and stabilize

        // Multiplication of 1100011101 and 1001011101
        dut.io.in1.valid.poke(true.B)
        dut.io.in2.valid.poke(true.B)
        dut.io.in1.bits.poke("b1100011101".U)
        dut.io.in2.bits.poke("b1100011101".U) 
        dut.clock.stepUntil(dut.io.out.valid, 1, 50)

        dut.io.out.bits.expect("b101101".U)
        dut.io.out.valid.expect(true.B)
      }
    }
   it("performs GF Power") {
      //simulate(new GFPower(fieldSize)) { dut =>
      simulate(new GFPower(fieldSize), buildDir = "build", enableWaves = true) { dut =>
        // 1100011101^2
        dut.io.in1.valid.poke(true.B)
        dut.io.in2.valid.poke(true.B)
        dut.io.in1.bits.poke("b1100011101".U)
        dut.io.in2.bits.poke("b10".U) 
        // while (!dut.io.out.valid) { dut.clock.step(1) }
        dut.clock.stepUntil(dut.io.out.valid, 1, 50)
        dut.io.out.bits.expect("b00101101".U)
        dut.io.out.valid.expect(true.B)

        dut.io.in1.valid.poke(false.B)  // Reset valid signal
        dut.io.in2.valid.poke(false.B)  // Reset valid signal
        dut.clock.step(3)  // Wait for module to return to idle and stabilize

        dut.io.in1.valid.poke(true.B)
        dut.io.in2.valid.poke(true.B)
        dut.io.in1.bits.poke("b1100011101".U)
        dut.io.in2.bits.poke("b11".U) 
        // while (!dut.io.out.valid) { dut.clock.step(1) }
        dut.clock.stepUntil(dut.io.out.valid, 1, 50)
        dut.io.out.bits.expect("b00001100".U)
        dut.io.out.valid.expect(true.B)
      }
    }
}
}