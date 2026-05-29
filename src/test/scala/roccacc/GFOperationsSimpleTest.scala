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

  describe("GFReduce") {
    it("performs GF Reduction") {
      simulate(new GFReduce(fieldSize), buildDir = "build", enableWaves = true, testName = Some("performs_GF_Reduction")) { dut =>
        // Reduction of 100011101
        dut.io.in1.valid.poke(true.B)
        dut.io.in1.bits.poke("b100011101".U)
        while (!dut.io.out.valid.peek().litToBoolean) {
          dut.clock.step()
        }
        dut.io.out.bits.expect("b00000000".U)
        dut.io.out.valid.expect(true.B)

        // Reset for second test
        dut.io.in1.valid.poke(false.B)
        dut.clock.step(2)

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
  }

  describe("GFAdd") {
    it("performs GF Addition") {
      simulate(new GFAdd(fieldSize), buildDir = "build", enableWaves = true, testName = Some("performs_GF_Addition")) { dut =>
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

        dut.io.in1.valid.poke(false.B)
        dut.io.in2.valid.poke(false.B)
        dut.clock.step(3)

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

        dut.io.in1.valid.poke(false.B)
        dut.io.in2.valid.poke(false.B)
        dut.clock.step(3)

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
  }

  describe("GFMul") {
    it("performs GF Multiplication") {
      simulate(new GFMul(fieldSize), buildDir = "build", enableWaves = true, testName = Some("performs_GF_Multiplication")) { dut =>
        // Multiplication of 1100011101 and 1001011101
        dut.io.in1.valid.poke(true.B)
        dut.io.in2.valid.poke(true.B)
        dut.io.in1.bits.poke("b1100011101".U)
        dut.io.in2.bits.poke("b1001011101".U)
        dut.clock.stepUntil(dut.io.out.valid, 1, 50)

        dut.io.out.bits.expect("b10010011".U)
        dut.io.out.valid.expect(true.B)

        dut.io.in1.valid.poke(false.B)
        dut.io.in2.valid.poke(false.B)
        dut.clock.step(3)

        // Multiplication of 1100011101 and 1100011101 (same operand)
        dut.io.in1.valid.poke(true.B)
        dut.io.in2.valid.poke(true.B)
        dut.io.in1.bits.poke("b1100011101".U)
        dut.io.in2.bits.poke("b1100011101".U)
        dut.clock.stepUntil(dut.io.out.valid, 1, 50)

        dut.io.out.bits.expect("b101101".U)
        dut.io.out.valid.expect(true.B)
      }
    }
  }

  describe("GFPower") {
    it("performs GF Power") {
      simulate(new GFPower(fieldSize), buildDir = "build", enableWaves = true, testName = Some("performs_GF_Power")) { dut =>
        // 1100011101^2
        dut.io.in1.valid.poke(true.B)
        dut.io.in2.valid.poke(true.B)
        dut.io.in1.bits.poke("b1100011101".U)
        dut.io.in2.bits.poke("b10".U)
        dut.clock.stepUntil(dut.io.out.valid, 1, 50)
        dut.io.out.bits.expect("b00101101".U)
        dut.io.out.valid.expect(true.B)

        dut.io.in1.valid.poke(false.B)
        dut.io.in2.valid.poke(false.B)
        dut.clock.step(3)

        dut.io.in1.valid.poke(true.B)
        dut.io.in2.valid.poke(true.B)
        dut.io.in1.bits.poke("b1100011101".U)
        dut.io.in2.bits.poke("b11".U)
        dut.clock.stepUntil(dut.io.out.valid, 1, 50)
        dut.io.out.bits.expect("b00001100".U)
        dut.io.out.valid.expect(true.B)
      }
    }
  }

  describe("GFDiv") {
    it("performs GF Division") {
      simulate(new GFDiv(fieldSize), buildDir = "build", enableWaves = true, testName = Some("performs_GF_Division")) { dut =>
        // 1100011101 / 1001011101
        dut.io.in1.valid.poke(true.B)
        dut.io.in2.valid.poke(true.B)
        dut.io.in1.bits.poke("b1100011101".U)
        dut.io.in2.bits.poke("b1001011101".U)
        dut.clock.stepUntil(dut.io.out.valid, 1, 6000)
        dut.io.out.bits.expect("b111001".U)
        dut.io.out.valid.expect(true.B)
      }
    }
  }

  describe("GFSquare") {
    it("performs GF Squaring") {
      simulate(new GFSquare(fieldSize), buildDir = "build", enableWaves = true, testName = Some("performs_GF_Squaring")) { dut =>
        // 0x02^2 = 0x04 (no reduction needed)
        dut.io.in1.valid.poke(true.B)
        dut.io.in1.bits.poke("b10".U)
        dut.clock.stepUntil(dut.io.out.valid, 1, 50)
        dut.io.out.bits.expect("b100".U)
        dut.io.out.valid.expect(true.B)

        dut.io.in1.valid.poke(false.B)
        dut.clock.step(3)

        // 0x10^2 = 0x1D (x^4 squared = x^8 mod p(x) = x^4+x^3+x^2+1)
        dut.io.in1.valid.poke(true.B)
        dut.io.in1.bits.poke("b10000".U)
        dut.clock.stepUntil(dut.io.out.valid, 1, 50)
        dut.io.out.bits.expect("b11101".U)
        dut.io.out.valid.expect(true.B)

        dut.io.in1.valid.poke(false.B)
        dut.clock.step(3)

        // 0x3A^2 = 0x2D (matches GFMul(0x3A, 0x3A) and GFPower(0x3A, 2))
        dut.io.in1.valid.poke(true.B)
        dut.io.in1.bits.poke("b111010".U)
        dut.clock.stepUntil(dut.io.out.valid, 1, 50)
        dut.io.out.bits.expect("b101101".U)
        dut.io.out.valid.expect(true.B)
      }
    }
  }
}
