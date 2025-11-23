package roccacc

import chisel3._
import chisel3.util._
import chisel3.simulator.EphemeralSimulator._
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

    it("initially has invalid flag") {
      simulate(new GFOperations(fieldSize)) { dut =>
        dut.io.valid.expect(false.B)
      }
    }

    it("performs GF Reduction") {
      simulate(new GFReduce(fieldSize)) { dut =>
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
      simulate(new GFAdd(fieldSize)) { dut =>
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
      simulate(new GFMul(fieldSize)) { dut =>
        // Multiplication of 1100011101 and 1001011101
        dut.io.in1.valid.poke(true.B)
        dut.io.in2.valid.poke(true.B)
        dut.io.in1.bits.poke("b1100011101".U)
        dut.io.in2.bits.poke("b1001011101".U) 
       while (!dut.io.out.valid.peek().litToBoolean) {
    dut.clock.step()
  }
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
       while (!dut.io.out.valid.peek().litToBoolean) {
    dut.clock.step()
  }
        dut.io.out.bits.expect("b101101".U)
        dut.io.out.valid.expect(true.B)

  //       // Reset for second test
  //       dut.io.in1.valid.poke(false.B)  // Reset valid signal
  //       dut.clock.step(3)  // Wait for module to return to idle and stabilize

  //      // Addition of 1100011101 and 100011101
  //       dut.io.in1.valid.poke(true.B)             
  //       dut.io.in1.bits.poke("b1100011101".U)  
  //       dut.io.in2.valid.poke(true.B)
  //       dut.io.in2.bits.poke("b100011101".U)
  //       while (!dut.io.out.valid.peek().litToBoolean) {
  //   dut.clock.step()
  // }
  //       dut.io.out.bits.expect("b00111010".U)
  //       dut.io.out.valid.expect(true.B)

  //        dut.io.in1.valid.poke(false.B)  // Reset valid signal
  //        dut.io.in2.valid.poke(false.B)  // Reset valid signal
  //       dut.clock.step(3)  // Wait for module to return to idle and stabilize

  //       // Addition of 1100101101 and 1100011101
  //       dut.io.in1.valid.poke(true.B)             
  //       dut.io.in1.bits.poke("b1100101101".U)  
  //       dut.io.in2.valid.poke(true.B)
  //       dut.io.in2.bits.poke("b1100011101".U)
  //       while (!dut.io.out.valid.peek().litToBoolean) {
  //   dut.clock.step()
  // }
  //       dut.io.out.bits.expect("b00110000".U)
  //       dut.io.out.valid.expect(true.B)
      }
    }

    // it("gets irreducible polynomial") {
    //   simulate(new GFOperations(fieldSize)) { dut =>
    //     dut.io.fn.poke(6.U) // FN_GIP = BitPat("b0110")
    //     dut.clock.step(1)
    //     dut.io.result.expect("b100011101".U)
    //     dut.io.valid.expect(true.B)
    //   }
    // }


  //   it("performs GF multiplication for irreducible polynomial") {
  //     simulate(new GFOperations(fieldSize)) { dut =>
  //       dut.io.fn.poke(1.U) // FN_MUL = BitPat("b0001")
  //       dut.io.operand1.poke("b10110111".U)
  //       dut.io.operand2.poke("b01010101".U)
  //       dut.clock.step(1)
  //       dut.io.result.expect("b00101000".U)
  //       dut.io.valid.expect(true.B)
  //     }
  //   }
  // }
}
}