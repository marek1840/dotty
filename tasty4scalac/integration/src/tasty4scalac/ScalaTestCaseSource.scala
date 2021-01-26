package tasty4scalac

import java.nio.file.{Files, Path, Paths}

import scala.collection.JavaConverters._


object ScalaTestCaseSource {
  private val scalaExtension = ".scala"
  private val root: Path = Paths.get(getClass.getClassLoader.getResource("test-cases").toURI)

  def testCases(): Seq[Path] = findTestCases(root)

  private def findTestCases(path: Path): Seq[Path] = {
    if (!Files.exists(path)) Nil
    else Files.walk(path)
      .iterator()
      .asScala
      .filter(_.getFileName.toString.endsWith(scalaExtension))
      .toSeq
  }

}
