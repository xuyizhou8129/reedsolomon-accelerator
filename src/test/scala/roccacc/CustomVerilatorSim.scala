package roccacc

import chisel3._
import chisel3.simulator.{PeekPokeAPI, SingleBackendSimulator}
import svsim.{CommonCompilationSettings, verilator}

object CustomVerilatorSim extends PeekPokeAPI {

  def simulate[T <: RawModule](
      module: => T,
      buildDir: String,
      enableWaves: Boolean = false
  )(body: (T) => Unit): Unit = {
    val simulator = makeSimulator(buildDir, enableWaves)
    simulator.simulate(module) { module =>
        module.controller.setTraceEnabled(enableWaves)
        body(module.wrapped)
      }
      .result

      simulator.cleanup()
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

  //   def cleanup(): Unit = {
  //       val tmpDir = os.Path(workspacePath)
  //       val workDir = os.pwd / os.RelPath(workspacePath)

  //       if (os.exists(workDir)) { os.remove.all(workDir) }
  //       os.move(tmpDir, workDir, replaceExisting = true, createFolders = true, atomicMove = true)
  //   }
  // }

 def cleanup(): Unit = {
    // workspacePath is already absolute, so use it directly
    val tmpDir = os.Path(workspacePath)
    val workDir = os.Path(workspacePath)  // Same location (or specify different destination if needed)
    
    if (os.exists(workDir)) { os.remove.all(workDir) }
    // If tmpDir and workDir are the same, no move is needed
    // Or if you need to move it somewhere else, specify the destination
}
  }
  private def makeSimulator(
      buildDir: String,
      enableWaves: Boolean
  ): DefaultSimulator = {
    val className = getClass.getName.stripSuffix("$")
    val id = java.lang.management.ManagementFactory.getRuntimeMXBean.getName

    //val tmpWorkspacePath = Seq(buildDir, className, id).mkString("/")
    val tmpWorkspacePath = (os.pwd / os.RelPath(Seq(buildDir, className, id).mkString("/"))).toString
    new DefaultSimulator(tmpWorkspacePath, enableWaves)
  }

}