package roccacc
import java.nio.file.Files
import java.nio.file.Paths
import java.util.UUID
import chisel3._
import chisel3.simulator.{PeekPokeAPI, SingleBackendSimulator}
import svsim.{CommonCompilationSettings, verilator}

object CustomVerilatorSim extends PeekPokeAPI {

  def simulate[T <: RawModule](
      module: => T,
      buildDir: String,
      enableWaves: Boolean = false,
      testName: Option[String] = None
  )(body: (T) => Unit): Unit = {
    val simulator = makeSimulator(buildDir, enableWaves, testName)
    simulator.simulate(module) { module =>
        module.controller.setTraceEnabled(enableWaves)
        body(module.wrapped)
      }
      .result

      //simulator.cleanup()
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
    
    // Use test name if provided, otherwise use a generic name
    val namePart = testName match {
      case Some(name) => 
        // Sanitize test name for filesystem: replace spaces and special chars with underscores
        name.replaceAll("[^a-zA-Z0-9]", "_").toLowerCase
      case None =>
        // Fall back to generic name if test name not provided
        "test"
    }
    
    // Add a short UUID suffix to ensure uniqueness for parallel test runs
    val uniqueId = UUID.randomUUID().toString.replace("-", "").take(6)
    //val uniqueId = DateTime.now(timezone.UTC).toIso
    val workspacePath = Seq(buildDir, className, s"${namePart}_${uniqueId}").mkString("/")
    val workspaceDir = Paths.get(os.pwd.toString, workspacePath)
    // Ensure the workspace directory exists
    Files.createDirectories(workspaceDir)
    new DefaultSimulator(workspaceDir.toString(), enableWaves)
  }

}