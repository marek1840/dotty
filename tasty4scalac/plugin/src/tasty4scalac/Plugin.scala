package tasty4scalac

import dotty.tools.dotc.core.tasty.TastyPrinter
import tasty.ScalacTastyWriter

import scala.tools.nsc.backend.jvm.PostProcessorFrontendAccess
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.tools.nsc.{Global, Phase}

class Plugin(val global: Global) extends NscPlugin {
  self =>
  val name = "tasty"
  val description = "Pickles Scala trees (tasty format)."
  val components = List[NscPluginComponent](TastyComponent)

  def changePhasesOrder(runsAfterPhase: String, phaseClass: Class[_], fieldToModify: Object) = {
    val newRunsAfter = List(runsAfterPhase)
    val runsAfterField = phaseClass.getDeclaredField("runsAfter")
    runsAfterField.setAccessible(true)
    runsAfterField.set(fieldToModify, newRunsAfter)
  }

  //update the order of phases
  //typer, superaccessors
  changePhasesOrder("typer", classOf[scala.tools.nsc.Global$superAccessors$], global.superAccessors)

  //superaccessors, patmat
  changePhasesOrder("superaccessors", classOf[scala.tools.nsc.Global$patmat$], global.patmat)

  //patmat, extensionMethods
  changePhasesOrder("patmat", classOf[scala.tools.nsc.Global$extensionMethods$], global.extensionMethods)


  object TastyComponent extends {
    implicit val global: self.global.type = self.global
  } with NscPluginComponent {
    override val runsAfter = List("superaccessors")
    override val runsRightAfter = Some("superaccessors")
    override val phaseName = "tasty"

    override def description = "pickle tasty trees"

    private val frontendAccess = new PostProcessorFrontendAccess.PostProcessorFrontendAccessImpl(global)

    import global._

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = {
        val tree = unit.body
        assert(!unit.isJava)

        tree match {
          case EmptyTree =>
          case PackageDef(_, ClassDef(_, name, _, _) :: Nil) => // TODO currently only single top-level class is supported
            writeTasty(unit, name)
          case ClassDef(_, name, _, _) => writeTasty(unit, name)
          case _ =>
        }
      }
    }

    private def writeTasty(unit: CompilationUnit, name: global.Name): Unit = {
      val tree = unit.body

      val writer = new ScalacTastyWriter()
      writer.write(tree)
      val bytes = writer.output()

      // write tasty to file
      val outputDirectory = frontendAccess.compilerSettings.outputDirectory(unit.source.file)
      val tastyFile = outputDirectory.fileNamed(name.decode + ".tasty")
      val out = tastyFile.output
      out.write(bytes)
      out.close()

      // print tasty
      val ctx = (new dotty.tools.dotc.core.Contexts.ContextBase).initialCtx
      val cnt = new TastyPrinter(bytes)(ctx).printContents()
      println(cnt)
    }
  }

}
