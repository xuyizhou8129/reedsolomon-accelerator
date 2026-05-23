package chisel3.simulator

import svsim._
import chisel3._

// Cycle-counting extension of PeekPokeAPI.
// Must live in chisel3.simulator for access to private[simulator] APIs.
//
// Key design:  `testableClock` extends `super.testableClock(clock)` so its type
// is a SUBTYPE of PeekPokeAPI's inner class.  That satisfies Scala 2's covariant-
// return-type override rule and makes `import CustomVerilatorSim._` deliver the
// counting version without ambiguity.
trait CycleCountingAPI extends PeekPokeAPI {

  // One ThreadLocal per concrete singleton; safe for ParallelTestExecution.
  val _cycleCount: ThreadLocal[Long] = new ThreadLocal[Long] {
    override def initialValue(): Long = 0L
  }

  def cycleCount: Long = _cycleCount.get()

  // Shadows PeekPokeAPI.testableClock with a subclass.  super.step() delegates
  // to the real Verilator tick; we only add the counting.
  implicit class testableClock(clock: Clock) extends super.testableClock(clock) {

    override def step(cycles: Int = 1): Unit = {
      if (cycles > 0) _cycleCount.set(_cycleCount.get() + cycles)
      super.step(cycles)
    }

    // Loop-based so every individual cycle is counted precisely.
    // The original sentinel-based tick cannot report how many cycles ran,
    // so we run step(1) in a loop and check the sentinel ourselves.
    override def stepUntil(sentinelPort: Data, sentinelValue: BigInt, maxCycles: Int): Unit = {
      var remaining = maxCycles
      while (remaining > 0) {
        step(1)
        remaining -= 1
        // peekValue() is testableData from PeekPokeAPI (not shadowed here).
        if (sentinelPort.peekValue().asBigInt == sentinelValue) return
      }
    }
  }
}
