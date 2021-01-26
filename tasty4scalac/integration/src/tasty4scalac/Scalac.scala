package tasty4scalac

import java.nio.file._
import java.util.UUID

import scala.reflect.internal.util.{BatchSourceFile, NoFile, SourceFile}
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.NoReporter
import scala.tools.nsc.{Global, Settings}

final class Scalac(settings: Settings) extends Global(settings, NoReporter) with Compiler {

  override def compile(code: String): Map[String, BinaryTasty] = {
    val file = new BatchSourceFile(NoFile, code.toCharArray)
    compile(file)
  }

  private def compile(files: SourceFile*): Map[String, BinaryTasty] = {
    val output = newOutputDirectory
    settings.outputDirs.setSingleOutput(output)
    new Run().compileSources(files.toList)
    findTastyFiles(output.iterator.toSeq, Map())
  }

  private def newOutputDirectory = new VirtualDirectory("tasty-scalac-" + UUID.randomUUID().toString, None)

  private def findTastyFiles(files: Seq[AbstractFile], acc: Map[String, BinaryTasty]): Map[String, BinaryTasty] = files match {
    case Seq() => acc

    case file +: tail if file.isDirectory =>
      val newFiles = file.iterator.toSeq
      findTastyFiles(newFiles ++ tail, acc)

    case file +: tail if file.name.endsWith(".tasty") =>
      findTastyFiles(tail, acc + (file.name -> BinaryTasty(file.toByteArray)))

    case _ +: tail => findTastyFiles(tail, acc)
  }
}

object Scalac {
  private val classpath = System.getProperty("scalac.classpath")
  private val pluginPath = System.getProperty("scalac.plugin.classpath")

  def apply(): Scalac = {
    val settings = new Settings()
    settings.classpath.value = classpath
    settings.plugin.value = List(pluginPath)
    settings.stopAfter.value = List("tasty")

    new Scalac(settings)
  }
}