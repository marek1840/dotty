package tasty4scalac

import java.util.UUID

import dotty.tools.dotc.core.Comments.{ContextDoc, ContextDocstrings}
import dotty.tools.dotc.core.Contexts
import dotty.tools.dotc.core.Contexts.ContextBase
import dotty.tools.dotc.reporting.Reporter.NoReporter
import dotty.tools.io.{AbstractFile, VirtualDirectory}

final class Dotty(val baseContext: Contexts.Context) extends dotty.tools.dotc.Compiler with Compiler {
  override def compile(code: String): Map[String, BinaryTasty] = {
    val ctx = baseContext.fresh

    val output = newOutputDirectory
    ctx.setSetting(ctx.settings.outputDir, output)
    newRun(ctx).compile(code)

    findTastyFiles(output.iterator().toSeq, Map())
  }

  private def newOutputDirectory = new VirtualDirectory("tasty-dotty-" + UUID.randomUUID().toString)

  private def findTastyFiles(files: Seq[AbstractFile], acc: Map[String, BinaryTasty]): Map[String, BinaryTasty] = files match {
    case Seq() => acc

    case file +: tail if file.isDirectory =>
      val newFiles = file.iterator().toSeq
      findTastyFiles(newFiles ++ tail, acc)

    case file +: tail if file.name.endsWith(".tasty") =>
      findTastyFiles(tail, acc + (file.name -> BinaryTasty(file.toByteArray)))

    case _ +: tail => findTastyFiles(tail, acc)
  }
}

object Dotty {
  private val classpath = System.getProperty("dotty.classpath")

  def apply(): Dotty = {
    val ctx = new ContextBase().initialCtx.fresh
    ctx.setSetting(ctx.settings.classpath, classpath)
    ctx.setSetting(ctx.settings.encoding, "UTF8")

    ctx.setReporter(NoReporter)
    ctx.setProperty(ContextDoc, new ContextDocstrings)
    new Dotty(ctx)
  }
}
