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

class CheckRootSimpleTest extends AnyFunSpec with ParallelTestExecution {

  val fieldSize = 8
  val numCoeffs = 15
  val roots     = Seq(1, 2, 3, 4)
  val numRoots  = roots.length

  // Memory layout for the write-only port (matching CheckRoots address map):
  //   [baseM .. baseM + numRoots*numCoeffs - 1]  matrix M, row-major  (written by HW)
  //   [baseS .. baseS + numRoots - 1]            syndrome vector S    (written by HW)
  val baseM   = 0
  val baseS   = numRoots * numCoeffs       // 4 * 15 = 60
  val memSize = baseS + numRoots           // 60 + 4 = 64

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

  def runCheckRootsTest(coeffs: Seq[BigInt], testName: String): Unit = {
    val sw = SWCheckRoots.checkRoots(coeffs, roots.map(BigInt(_)))

    println(s"[$testName] coeffs       = ${coeffs.mkString(", ")}")
    println(s"[$testName] SW corrupted = ${sw.corrupted}")
    println(s"[$testName] SW sVals     = ${sw.sVals.mkString(", ")}")

    val mem = Array.fill(memSize)(BigInt(0))

    simulate(
      new CheckRoots(numCoeffs, roots),
      buildDir = "build",
      enableWaves = true,
      testName = Some(testName)
    ) { dut =>
      // Coefficients are stable inputs — poke once and leave
      for (j <- 0 until numCoeffs)
        dut.io.coeffs(j).poke(coeffs(j).U(fieldSize.W))

      dut.io.baseM.poke(baseM.U)
      dut.io.baseS.poke(baseS.U)
      dut.io.memReady.poke(true.B)
      dut.io.start.poke(false.B)
      dut.clock.step(2)

      // Pulse start for one cycle
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // Drive write-only memory port until done pulses
      // (same while-loop pattern as MatsolveSimpleTest)
      var isDone = false
      var cycles = 0
      val maxCycles = 1000000

      while (!isDone && cycles < maxCycles) {
        if (dut.io.memWrite.peek().litToBoolean) {
          val addr = dut.io.memAddr.peek().litValue.toInt
          require(addr >= 0 && addr < memSize,
            s"[$testName] write addr $addr out of range [0,$memSize)")
          mem(addr) = dut.io.memWData.peek().litValue
        }

        if (dut.io.done.peek().litToBoolean) {
          isDone = true
        } else {
          dut.clock.step(1)
          cycles += 1
        }
      }

      assert(isDone, s"[$testName] timed out after $maxCycles cycles")
      println(s"[$testName] completed in $cycles cycles")

      // corrupted is valid in the done cycle
      val hwCorrupted = dut.io.corrupted.peek().litToBoolean
      println(s"[$testName] HW corrupted = $hwCorrupted")
      assert(hwCorrupted == sw.corrupted,
        s"[$testName] corrupted mismatch: HW=$hwCorrupted SW=${sw.corrupted}")

      // Matrix values written to mem by HW (row-major at baseM)
      println(s"[$testName] Checking Vandermonde matrix M (baseM=$baseM):")
      for (i <- 0 until numRoots; j <- 0 until numCoeffs) {
        val hwVal = mem(baseM + i * numCoeffs + j)
        val swVal = sw.mat(i)(j)
        println(s"  M[$i][$j]: HW=$hwVal SW=$swVal")
        assert(hwVal == swVal,
          s"[$testName] M[$i][$j] mismatch: HW=$hwVal SW=$swVal")
      }

      // Syndrome values written to mem by HW (at baseS)
      println(s"[$testName] Checking syndromes S (baseS=$baseS):")
      for (i <- 0 until numRoots) {
        val hwVal = mem(baseS + i)
        val swVal = sw.sVals(i)
        println(s"  S[$i]: HW=$hwVal SW=$swVal")
        assert(hwVal == swVal,
          s"[$testName] S[$i] mismatch: HW=$hwVal SW=$swVal")
      }

      println(s"[$testName] PASSED")
    }
  }

  describe("CheckRoots HW tested against SW golden model") {

    it("detects no corruption for a valid RS(15,11) codeword") {
      val message  = (2 to 12).map(BigInt(_))
      val codeword = SWModel.encode(message, 15, 11, 8)
      runCheckRootsTest(codeword, "valid_codeword_rs15_11")
    }

    it("detects corruption when first symbol is altered") {
      val message  = (2 to 12).map(BigInt(_))
      val codeword = SWModel.encode(message, 15, 11, 8).toArray
      codeword(0)  = codeword(0) ^ BigInt(5)
      runCheckRootsTest(codeword.toSeq, "corrupted_first_symbol")
    }

    it("detects no corruption for an all-zero polynomial") {
      runCheckRootsTest(Seq.fill(numCoeffs)(BigInt(0)), "all_zeros")
    }

    it("detects corruption when a middle symbol is altered") {
      val message  = (2 to 12).map(BigInt(_))
      val codeword = SWModel.encode(message, 15, 11, 8).toArray
      codeword(7)  = codeword(7) ^ BigInt(13)
      runCheckRootsTest(codeword.toSeq, "corrupted_middle_symbol")
    }
  }
}
