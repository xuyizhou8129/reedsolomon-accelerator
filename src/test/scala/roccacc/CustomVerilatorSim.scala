package roccacc

import java.nio.file.Files
import java.nio.file.Paths
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import chisel3._
import chisel3.simulator.{CycleCountingAPI, SingleBackendSimulator}
import chisel3.simulator.Simulator.{SimulationDigest, CompilationFailed}
import svsim.{CommonCompilationSettings, verilator}

object CustomVerilatorSim extends CycleCountingAPI {

  def simulate[T <: RawModule](
      module: => T,
      buildDir: String,
      enableWaves: Boolean = false,
      testName: Option[String] = None
  )(body: (T) => Unit): Unit = {
    _cycleCount.set(0L)
    val simulator = makeSimulator(buildDir, enableWaves, testName)

    // Retain the full BackendInvocationDigest from Simulator.scala so we can
    // read compilationEndTime and the SimulationDigest timestamps.
    val digest = simulator.simulate(module) { m =>
      m.controller.setTraceEnabled(enableWaves)
      body(m.wrapped)
    }

    val totalCycles = _cycleCount.get()
    val compileMs   = (digest.compilationEndTime - digest.compilationStartTime) / 1_000_000L

    digest.outcome match {
      case SimulationDigest(simStart, simEnd, outcome) =>
        val simMs      = (simEnd - simStart) / 1_000_000L
        val throughput = if (simMs > 0) f"${totalCycles * 1000L / simMs}%,d cyc/s" else "N/A"
        println(
          s"[CycleCount] compile=${compileMs}ms  " +
          s"sim=${simMs}ms  " +
          s"cycles=${totalCycles}  " +
          s"throughput=${throughput}"
        )
        outcome.get   // re-raise any simulation exception
      case CompilationFailed(error) =>
        println(s"[CycleCount] compilation failed after ${compileMs}ms")
        throw error
    }
  }

  private class DefaultSimulator(
      val workspacePath: String,
      enableWaves: Boolean = false
  ) extends SingleBackendSimulator[verilator.Backend] {
    val backend = verilator.Backend.initializeFromProcessEnvironment()
    val tag = "default"
    val commonCompilationSettings = CommonCompilationSettings()
    val backendSpecificCompilationSettings = {
      val settings = verilator.Backend.CompilationSettings()
      if (enableWaves) {
        settings.copy(
          traceStyle = Some(verilator.Backend.CompilationSettings.TraceStyle.Vcd(traceUnderscore = false))
        )
      } else {
        settings
      }
    }
  }

  private def makeSimulator(
      buildDir: String,
      enableWaves: Boolean,
      testName: Option[String]
  ): DefaultSimulator = {
    val className = getClass.getName.stripSuffix("$")
    val namePart = testName match {
      case Some(name) => name.replaceAll("[^a-zA-Z0-9]", "_").toLowerCase
      case None       => "test"
    }
    val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss_SSS")
    val uniqueId = LocalDateTime.now().format(dateTimeFormatter)
    val workspacePath = Seq(buildDir, className, s"${namePart}_${uniqueId}").mkString("/")
    val workspaceDir = Paths.get(os.pwd.toString, workspacePath)
    Files.createDirectories(workspaceDir)
    new DefaultSimulator(workspaceDir.toString(), enableWaves)
  }
}
