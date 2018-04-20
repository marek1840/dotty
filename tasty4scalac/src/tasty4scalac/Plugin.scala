package tasty4scalac

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.collection.mutable
import scala.tools.nsc.SubComponent
import scala.tools.nsc.Phase
import scala.tools.nsc.backend.jvm.GenBCode
import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.transform.Erasure

import dotty.tools.dotc.core.tasty.TastyPrinter

import java.io._
import java.nio.file._

class Plugin(val global: Global) extends NscPlugin { self =>
  val name = "tasty"
  val description = "Pickles Scala trees (tasty format)."
  val components = List[NscPluginComponent](TastyComponent)

  object TastyComponent extends {
    val global: self.global.type = self.global
  } with NscPluginComponent {
    override val runsAfter = List("pickler")
    val phaseName = "tasty"
    override def description = "pickle tasty trees"

    import global._

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = {
        val tree = unit.body
        assert(!unit.isJava)

        println("Hello: " + tree)
        val pickler = new ScalacTastyPickler(global)
        val treePkl = pickler.treePkl
        treePkl.spickle(List(tree.asInstanceOf[treePkl.g.Tree]))

        treePkl.compactify()
        val pickled = pickler.assembleParts()

        val ctx = (new dotty.tools.dotc.core.Contexts.ContextBase).initialCtx
        new TastyPrinter(pickled)(ctx).printContents()

        val path = Paths.get("Test.tasty")
        Files.write(path, pickled)

        // dotty.tools.dotc.core.Main.process("-from-tasty"
      }
    }
  }
}
