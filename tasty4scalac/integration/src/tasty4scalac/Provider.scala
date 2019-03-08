package tasty4scalac

import java.nio.file.{Files, Path, Paths}
import java.util
import java.util.regex.Pattern
import scala.collection.JavaConverters._

trait Provider {
  def testCases(): util.Collection[Array[CompileSource]]
}

object Providers {
  private val root = Paths.get(System.getProperty("test.root.directory"))

  def compiledSourceProvider: Provider = new CompiledSources(root)

  private final class CompiledSources(root: Path) extends Provider {
    private val scalaExtension = ".scala"

    private val negativeTests = Seq(
      Pattern.compile(".*/neg(-.*)?/.*"),
      Pattern.compile(".*/run-with-compiler/.*"),
      Pattern.compile(".*/vulpix-tests/.*"),
      Pattern.compile(".*/disabled/.*"),
    )

    def testCases(): util.Collection[Array[CompileSource]] = {
      findTestCases(root)
        .map(createTestCase)
        .map(Array(_))
        .asJavaCollection
    }

    private def findTestCases(path: Path): Seq[Path] = {
      if (!Files.exists(path)) Nil
      else Files.walk(path)
        .iterator()
        .asScala
        .filterNot(path => negativeTests.exists(p => p.matcher(path.toString).matches()))
        .filter(_.getFileName.toString.endsWith(scalaExtension))
        .toSeq
    }

    private def createTestCase(path: Path): CompileSource = {
      val name = root.relativize(path).toString.dropRight(scalaExtension.length)
      new CompileSource(name, path)
    }
  }
}
