package roccacc

import chisel3._
import chisel3.util._
import CustomVerilatorSim._
import org.scalatest.funspec.AnyFunSpec

/* How to run this test
 *
 * From the Chipyard root (accelerator/), inside the nix shell. The nix shell
 * is required: the system Verilator is too old for the svsim test driver.
 *
 *   cd /pool/xuyi/reed-solomon_fec/chipyardfork/accelerator
 *   nix develop
 *   sbt "roccacc/testOnly roccacc.RSUnitSimpleTest"
 *
 * Run a single case by matching a substring of its name with -z
 * (one RSUnit case; other cases: encodes a message, decodes a two-error):
 *
 *   sbt "roccacc/testOnly roccacc.RSUnitSimpleTest -- -z \"back to back\""
 *
 * The first run compiles rocket-chip and is slow. For repeated runs, start
 * `sbt` once and type the testOnly command at its prompt.
 *
 * Each case writes a VCD into build/ under the rocc-acc project, in a
 * directory named after the testName passed to simulate().
 */

/** Checks that RSUnit steers start/done correctly to the encoder and the
  * decoder and latches their results. The RS math itself is covered by
  * RSEncoderSimpleTest and RSDecoderSimpleTest. */
class RSUnitSimpleTest extends AnyFunSpec {

  val fieldSize = 8
  val n         = 15
  val k         = 11
  val roots     = Seq(1, 2, 3, 4)

  case class Result(out: Seq[BigInt], corrupted: Boolean, solved: Boolean, cycles: Int)

  /** Drive one operation through the unit and collect its results. */
  def runOp(dut: RSUnit, op: UInt, in: Seq[BigInt], testName: String): Result = {
    assert(!dut.io.busy.peek().litToBoolean, s"[$testName] unit busy before start")

    for (j <- 0 until n) dut.io.in(j).poke(in(j).U(fieldSize.W))
    dut.io.op.poke(op)
    dut.io.start.poke(true.B)
    dut.clock.step(1)
    dut.io.start.poke(false.B)
    // Inputs are latched on start; scrub them to prove the unit does not
    // depend on them afterwards.
    for (j <- 0 until n) dut.io.in(j).poke(0.U)
    dut.io.op.poke(0.U)

    var isDone    = false
    var cycles    = 0
    val maxCycles = 20000000
    while (!isDone && cycles < maxCycles) {
      if (dut.io.done.peek().litToBoolean) isDone = true
      else { dut.clock.step(1); cycles += 1 }
    }
    assert(isDone, s"[$testName] timed out after $maxCycles cycles")
    assert(dut.io.busy.peek().litToBoolean, s"[$testName] busy low during done pulse")

    val out       = (0 until n).map(j => dut.io.out(j).peek().litValue)
    val corrupted = dut.io.corrupted.peek().litToBoolean
    val solved    = dut.io.solved.peek().litToBoolean

    // done must be a single-cycle pulse and results must hold afterwards
    dut.clock.step(1)
    assert(!dut.io.done.peek().litToBoolean, s"[$testName] done held for more than one cycle")
    assert(!dut.io.busy.peek().litToBoolean, s"[$testName] busy still high after done")
    for (j <- 0 until n)
      assert(dut.io.out(j).peek().litValue == out(j), s"[$testName] out($j) changed after done")

    println(s"[$testName] cycles=$cycles corrupted=$corrupted solved=$solved out=${out.mkString(", ")}")
    Result(out, corrupted, solved, cycles)
  }

  describe("RSUnit steering encoder and decoder") {

    it("encodes a message and returns the codeword") {
      val message  = (2 to 12).map(BigInt(_))
      val expected = SWModel.encode(message, n, k, fieldSize)
      val in       = message ++ Seq.fill(n - k)(BigInt(0))

      simulate(new RSUnit(n, k, roots), buildDir = "build", enableWaves = true,
               testName = Some("rsunit_encode")) { dut =>
        val r = runOp(dut, RSUnit.OP_ENCODE, in, "rsunit_encode")
        assert(!r.corrupted && !r.solved, "encode must clear corrupted/solved")
        for (j <- 0 until n)
          assert(r.out(j) == expected(j), s"codeword[$j]: HW=${r.out(j)} SW=${expected(j)}")
      }
    }

    it("decodes a two-error codeword and returns the error vector") {
      val message  = (2 to 12).map(BigInt(_))
      val received = SWModel.encode(message, n, k, fieldSize).toArray
      received(0)  = received(0) ^ BigInt(5)
      received(11) = received(11) ^ BigInt(13)
      val sw = SWDecoder.decode(received.toSeq, n, k, roots.map(BigInt(_)))

      simulate(new RSUnit(n, k, roots), buildDir = "build", enableWaves = true,
               testName = Some("rsunit_decode")) { dut =>
        val r = runOp(dut, RSUnit.OP_DECODE, received.toSeq, "rsunit_decode")
        assert(r.corrupted == sw.corrupted, s"corrupted: HW=${r.corrupted} SW=${sw.corrupted}")
        assert(r.solved == sw.solved, s"solved: HW=${r.solved} SW=${sw.solved}")
        for (j <- 0 until n)
          assert(r.out(j) == sw.errorVec(j), s"errorVec[$j]: HW=${r.out(j)} SW=${sw.errorVec(j)}")
      }
    }

    it("runs encode then decode back to back on one instance") {
      val message = (2 to 12).map(BigInt(_))
      val in      = message ++ Seq.fill(n - k)(BigInt(0))

      simulate(new RSUnit(n, k, roots), buildDir = "build", enableWaves = true,
               testName = Some("rsunit_encode_then_decode")) { dut =>
        val enc = runOp(dut, RSUnit.OP_ENCODE, in, "rsunit_e2d_encode")
        val expected = SWModel.encode(message, n, k, fieldSize)
        for (j <- 0 until n)
          assert(enc.out(j) == expected(j), s"codeword[$j]: HW=${enc.out(j)} SW=${expected(j)}")

        // Corrupt one symbol of the hardware codeword and decode it
        val received = enc.out.toArray
        received(3)  = received(3) ^ BigInt(9)
        val sw  = SWDecoder.decode(received.toSeq, n, k, roots.map(BigInt(_)))
        val dec = runOp(dut, RSUnit.OP_DECODE, received.toSeq, "rsunit_e2d_decode")
        assert(dec.corrupted && dec.solved, "single error should be corrected")
        assert(dec.solved == sw.solved && dec.corrupted == sw.corrupted)
        for (j <- 0 until n)
          assert(dec.out(j) == sw.errorVec(j), s"errorVec[$j]: HW=${dec.out(j)} SW=${sw.errorVec(j)}")
        // received XOR errorVec must give back the clean codeword
        for (j <- 0 until n)
          assert((received(j) ^ dec.out(j)) == expected(j), s"corrected[$j] mismatch")
      }
    }
  }
}
