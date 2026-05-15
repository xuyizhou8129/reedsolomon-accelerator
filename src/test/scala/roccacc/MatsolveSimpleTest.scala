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

class MatsolveSimpleTest extends AnyFunSpec with ParallelTestExecution {

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

  val rows = 4
  val cols = 4

  // Memory layout: A at baseA (row-major), b at baseB, x written to baseX
  val baseA = 0
  val baseB = rows * cols
  val baseX = rows * cols + rows
  val memSize = baseX + cols

  def runMatSolveTest(
    A: Seq[Seq[BigInt]],
    b: Seq[BigInt],
    testName: String
  ): Unit = {
    // Software golden model
    val swResult = SWMatSolve.solve(A, b)
    require(swResult.isDefined, s"SW model returned None for $testName — check test inputs")
    val expected = swResult.get

    println(s"[$testName] A = ${A.map(_.mkString("[", ",", "]")).mkString("[", ", ", "]")}")
    println(s"[$testName] b = ${b.mkString("[", ",", "]")}")
    println(s"[$testName] SW x = ${expected.mkString("[", ",", "]")}")

    // Populate memory with A and b
    val mem = Array.fill(memSize)(BigInt(0))
    for (i <- 0 until rows; j <- 0 until cols)
      mem(baseA + i * cols + j) = A(i)(j)
    for (i <- 0 until rows)
      mem(baseB + i) = b(i)

    simulate(new MatSolve(rows, cols), buildDir = "build", enableWaves = true,
      testName = Some(testName)) { dut =>

      dut.io.baseA.poke(baseA.U)
      dut.io.baseB.poke(baseB.U)
      dut.io.baseX.poke(baseX.U)
      dut.io.memReady.poke(true.B)
      dut.io.memRData.poke(0.U)
      dut.io.start.poke(false.B)
      dut.clock.step(2)

      // Pulse start for one cycle
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // Drive memory interface until done signal is observed
      var isDone = false
      var cycles = 0
      val maxCycles = 100000

      while (!isDone && cycles < maxCycles) {
        val addr = dut.io.memAddr.peek().litValue.toInt

        if (dut.io.memRead.peek().litToBoolean) {
          require(addr >= 0 && addr < memSize, s"Read address $addr out of range")
          dut.io.memRData.poke(mem(addr).U)
        }

        if (dut.io.memWrite.peek().litToBoolean) {
          require(addr >= 0 && addr < memSize, s"Write address $addr out of range")
          mem(addr) = dut.io.memWData.peek().litValue
        }

        // Capture done before stepping (done is a 1-cycle pulse)
        if (dut.io.done.peek().litToBoolean) {
          isDone = true
        } else {
          dut.clock.step(1)
          cycles += 1
        }
      }

      assert(isDone, s"[$testName] timed out after $maxCycles cycles")
      println(s"[$testName] completed in $cycles cycles")

      // Compare HW result (written to mem by storeX) against SW golden
      for (i <- 0 until cols) {
        val hwVal = mem(baseX + i)
        val swVal = expected(i)
        println(s"  x[$i]: HW=$hwVal, SW=$swVal")
        assert(hwVal == swVal,
          s"[$testName] mismatch at x[$i]: HW=$hwVal, SW=$swVal")
      }

      println(s"[$testName] PASSED: HW result matches SW golden model")
    }
  }

  describe("MatSolve HW tested against SW golden model") {

    it("solves a 4x4 identity system in GF(2^8)") {
      // A = I, solution x = b
      val A = Seq(
        Seq(BigInt(1), BigInt(0), BigInt(0), BigInt(0)),
        Seq(BigInt(0), BigInt(1), BigInt(0), BigInt(0)),
        Seq(BigInt(0), BigInt(0), BigInt(1), BigInt(0)),
        Seq(BigInt(0), BigInt(0), BigInt(0), BigInt(1))
      )
      val b = Seq(BigInt(3), BigInt(5), BigInt(7), BigInt(11))
      runMatSolveTest(A, b, "identity_4x4")
    }

    it("solves a non-trivial 4x4 GF(2^8) system") {
      // Non-identity full-rank matrix; SW model computes expected x
      val A = Seq(
        Seq(BigInt(2),  BigInt(3),  BigInt(1),  BigInt(5)),
        Seq(BigInt(4),  BigInt(1),  BigInt(2),  BigInt(3)),
        Seq(BigInt(1),  BigInt(5),  BigInt(3),  BigInt(2)),
        Seq(BigInt(3),  BigInt(2),  BigInt(6),  BigInt(4))
      )
      val b = Seq(BigInt(10), BigInt(20), BigInt(30), BigInt(40))
      runMatSolveTest(A, b, "nontrivial_4x4")
    }

    it("detects an unsolvable system") {
      // Row 3 = all-zero A with non-zero b -> inconsistent
      val A = Seq(
        Seq(BigInt(1), BigInt(0), BigInt(0), BigInt(0)),
        Seq(BigInt(0), BigInt(1), BigInt(0), BigInt(0)),
        Seq(BigInt(0), BigInt(0), BigInt(1), BigInt(0)),
        Seq(BigInt(0), BigInt(0), BigInt(0), BigInt(0))  // zero row
      )
      val b = Seq(BigInt(1), BigInt(2), BigInt(3), BigInt(7))  // b[3] != 0

      val mem = Array.fill(memSize)(BigInt(0))
      for (i <- 0 until rows; j <- 0 until cols)
        mem(baseA + i * cols + j) = A(i)(j)
      for (i <- 0 until rows)
        mem(baseB + i) = b(i)

      simulate(new MatSolve(rows, cols), buildDir = "build", enableWaves = true,
        testName = Some("unsolvable_4x4")) { dut =>

        dut.io.baseA.poke(baseA.U)
        dut.io.baseB.poke(baseB.U)
        dut.io.baseX.poke(baseX.U)
        dut.io.memReady.poke(true.B)
        dut.io.memRData.poke(0.U)
        dut.io.start.poke(false.B)
        dut.clock.step(2)

        dut.io.start.poke(true.B)
        dut.clock.step(1)
        dut.io.start.poke(false.B)

        var isDone = false
        var cycles = 0
        val maxCycles = 100000

        while (!isDone && cycles < maxCycles) {
          val addr = dut.io.memAddr.peek().litValue.toInt
          if (dut.io.memRead.peek().litToBoolean) {
            dut.io.memRData.poke(mem(addr).U)
          }
          if (dut.io.done.peek().litToBoolean) {
            isDone = true
          } else {
            dut.clock.step(1)
            cycles += 1
          }
        }

        assert(isDone, "unsolvable_4x4: timed out")
        assert(dut.io.unsolvable.peek().litToBoolean,
          "unsolvable_4x4: expected unsolvable=true but got false")
        println("unsolvable_4x4 PASSED: HW correctly flagged system as unsolvable")
      }
    }
  }
}
