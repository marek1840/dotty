package tasty4scalac

import java.nio.file._
import java.util.UUID

import scala.reflect.internal.util.{BatchSourceFile, NoFile, SourceFile}
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.NoReporter
import scala.tools.nsc.{Global, Settings}

final class Scalac(settings: Settings) extends Global(settings, NoReporter) with Compiler {
  override def compile(code: String): Set[Tasty] = {
    val file = new BatchSourceFile(NoFile, code.toCharArray)
    compile(file)
  }

  private def compile(files: SourceFile*): Set[Tasty] = {
    val output = newOutputDirectory
    settings.outputDirs.setSingleOutput(output)
    new Run().compileSources(files.toList)
    findTastyFiles(output.iterator.toSeq, Set())
  }

  private def newOutputDirectory = new VirtualDirectory("tasty-scalac-" + UUID.randomUUID().toString, None)

  private def findTastyFiles(files: Seq[AbstractFile], acc: Set[Tasty]): Set[Tasty] = files match {
    case Seq() => acc

    case file +: tail if file.isDirectory =>
      val newFiles = file.iterator.toSeq
      findTastyFiles(newFiles ++ tail, acc)

    case file +: tail if file.name.endsWith(".tasty") =>
      findTastyFiles(tail, acc + Tasty(file.toByteArray))

    case _ +: tail => findTastyFiles(tail, acc)
  }
}

object Scalac {
  private val classpath = "/home/tgodzik/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.12.8.jar:/home/tgodzik/.ivy2/cache/org.scala-lang/scala-compiler/jars/scala-compiler-2.12.8.jar:/home/tgodzik/.ivy2/cache/org.scala-lang/scala-reflect/jars/scala-reflect-2.12.8.jar" // System.getProperty("scalac.classpath")

  private val pluginPath = "/home/tgodzik/Documents/dotty/tasty4scalac/plugin/target/scala-2.12/tasty4scalac-plugin_2.12-0.14.0-bin-SNAPSHOT.jar:/home/tgodzik/Documents/dotty/library/target/scala-2.12/dotty-library_2.12-0.14.0-bin-SNAPSHOT-nonbootstrapped.jar:/home/tgodzik/Documents/dotty/compiler/target/scala-2.12/dotty-compiler_2.12-0.14.0-bin-SNAPSHOT-nonbootstrapped.jar:/home/tgodzik/Documents/dotty/interfaces/target/dotty-interfaces-0.14.0-bin-SNAPSHOT.jar" //  System.getProperty("scalac.plugin.classpath")
  def apply(): Scalac = {
    val settings = new Settings()
    settings.classpath.value = classpath
    settings.plugin.value = List(pluginPath)
    settings.stopAfter.value = List("tasty")

    new Scalac(settings)
  }
}