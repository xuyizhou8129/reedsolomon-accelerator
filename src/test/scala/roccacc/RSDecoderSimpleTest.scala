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

class RSDecoderSimpleTest extends AnyFunSpec with ParallelTestExecution {

  val fieldSize = 8
  val n         = 15
  val k         = 11
  val t         = (n - k) / 2   // 2
  val roots     = Seq(1, 2, 3, 4)
  val numRoots  = roots.length

  // Compact memory layout (one GF element per word):
  //   baseM        : Vandermonde M (numRoots × n = 60 words) — CheckRoots output, read-only
  //   baseS        : syndrome S (numRoots = 4 words)          — CheckRoots output, master copy
  //   baseAcompact : compact trial A (numRoots × t = 8 words) — gather phase
  //   baseB        : b copy (numRoots = 4 words)              — restored before each MatSolve
  //   baseXbuf     : compact x (t = 2 words)                  — MatSolve output
  //   baseX        : full error vector (n = 15 words)         — scattered after success
  val baseM        = 0
  val baseS        = numRoots * n                  // 60
  val baseAcompact = baseS + numRoots              // 64
  val baseB        = baseAcompact + numRoots * t   // 72
  val baseXbuf     = baseB + numRoots              // 76
  val baseX        = baseXbuf + t                  // 78
  val memSize      = baseX + n                     // 93

  implicit val p: Parameters = new Config((site, here, up) => {
    case TileKey => new TileParams {
      val core              = RocketCoreParams(nPMPs = 0)
      val icache            = None
      val dcache            = None
      val btb               = None
      val tileId            = 0
      val blockerCtrlAddr   = None
      val baseName          = "test_tile"
      val clockSinkParams   = freechips.rocketchip.prci.ClockSinkParameters()
      val uniqueName        = "test_tile_0"
    }
  })

  def runDecoderTest(received: Seq[BigInt], testName: String): Unit = {
    val sw = SWDecoder.decode(received, n, k, roots.map(BigInt(_)))

    println(s"[$testName] received    = ${received.mkString(", ")}")
    println(s"[$testName] SW corrupted= ${sw.corrupted}  solved=${sw.solved}")
    if (sw.solved)
      println(s"[$testName] SW errorVec = ${sw.errorVec.mkString(", ")}")

    val mem = Array.fill(memSize)(BigInt(0))

    simulate(
      new Coordinator(n, k, roots),
      buildDir  = "build",
      enableWaves = true,
      testName  = Some(testName)
    ) { dut =>

      // Poke received coefficients (held stable throughout)
      for (j <- 0 until n)
        dut.io.coeffs(j).poke(received(j).U(fieldSize.W))

      dut.io.baseM.poke(baseM.U)
      dut.io.baseS.poke(baseS.U)
      dut.io.baseAcompact.poke(baseAcompact.U)
      dut.io.baseB.poke(baseB.U)
      dut.io.baseXbuf.poke(baseXbuf.U)
      dut.io.baseX.poke(baseX.U)
      dut.io.memReady.poke(true.B)
      dut.io.memRData.poke(0.U)
      dut.io.start.poke(false.B)
      dut.clock.step(2)

      // Pulse start for one cycle
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // Drive the memory interface until done fires.
      var isDone    = false
      var cycles    = 0
      val maxCycles = 20000000

      while (!isDone && cycles < maxCycles) {
        val addr = dut.io.memAddr.peek().litValue.toInt

        if (dut.io.memRead.peek().litToBoolean) {
          require(addr >= 0 && addr < memSize,
            s"[$testName] read addr $addr out of range [0,$memSize)")
          dut.io.memRData.poke(mem(addr).U(fieldSize.W))
        } else {
          dut.io.memRData.poke(0.U)
        }

        if (dut.io.memWrite.peek().litToBoolean) {
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

      // Compare corrupted and solved flags
      val hwCorrupted = dut.io.corrupted.peek().litToBoolean
      val hwSolved    = dut.io.solved.peek().litToBoolean
      println(s"[$testName] HW corrupted=$hwCorrupted  solved=$hwSolved")

      assert(hwCorrupted == sw.corrupted,
        s"[$testName] corrupted mismatch: HW=$hwCorrupted SW=${sw.corrupted}")
      assert(hwSolved == sw.solved,
        s"[$testName] solved mismatch: HW=$hwSolved SW=${sw.solved}")

      // When solved, compare the error vector scattered to baseX by Coordinator
      if (sw.solved) {
        println(s"[$testName] Checking error vector x at baseX=$baseX:")
        for (j <- 0 until n) {
          val hwX = mem(baseX + j)
          val swX = sw.errorVec(j)
          println(s"  x[$j]: HW=$hwX  SW=$swX")
          assert(hwX == swX,
            s"[$testName] x[$j] mismatch: HW=$hwX SW=$swX")
        }
      }

      println(s"[$testName] PASSED")
    }
  }

  describe("Coordinator HW tested against SWDecoder golden model") {

    it("passes a valid RS(15,11) codeword without correction") {
      val message  = (2 to 12).map(BigInt(_))
      val codeword = SWModel.encode(message, n, k, fieldSize)
      runDecoderTest(codeword, "valid_codeword")
    }

    it("corrects a single-symbol error") {
      val message  = (2 to 12).map(BigInt(_))
      val received = SWModel.encode(message, n, k, fieldSize).toArray
      received(3)  = received(3) ^ BigInt(7)
      runDecoderTest(received.toSeq, "one_error_pos3")
    }

    it("corrects two symbol errors") {
      val message  = (2 to 12).map(BigInt(_))
      val received = SWModel.encode(message, n, k, fieldSize).toArray
      received(0)  = received(0) ^ BigInt(5)
      received(11) = received(11) ^ BigInt(13)
      runDecoderTest(received.toSeq, "two_errors_pos0_11")
    }

    it("reports failure for three symbol errors (uncorrectable)") {
      val message  = (2 to 12).map(BigInt(_))
      val received = SWModel.encode(message, n, k, fieldSize).toArray
      received(0)  = received(0)  ^ BigInt(1)
      received(5)  = received(5)  ^ BigInt(2)
      received(10) = received(10) ^ BigInt(3)
      runDecoderTest(received.toSeq, "three_errors_uncorrectable")
    }
  }
}
