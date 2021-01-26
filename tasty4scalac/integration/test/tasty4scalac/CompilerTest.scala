package tasty4scalac

import java.nio.file.{Files, Path}

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameters
import tasty.binary.BinaryInput
import tasty.{TastyADT, TastyADTUnpickler}

@RunWith(classOf[Parameterized])
class CompilerTest(fileName: String, scalacOutput: Map[String, TastyADT], dottyOutput: Map[String, TastyADT]) {

  @Test
  def scalacEmitsSameAmountOfTastyFiles(): Unit =
    assertEquals(dottyOutput.keySet, scalacOutput.keySet)

  @Test
  def scalacEmitsSubsetOfDottyEmittedNames(): Unit = {
    for {
      (name, scalacTasty) <- scalacOutput
      dottyTasty = dottyOutput(name)
      scalacEmittedNames = scalacTasty.names.toSet
      dottyEmittedNames = dottyTasty.names.toSet
      missingNames = scalacEmittedNames -- dottyEmittedNames
    } assertTrue(s"scalac emitted non-matching names for: $name. Those are: [$missingNames]", missingNames.isEmpty)
  }
}

object CompilerTest {

  import collection.JavaConverters._

  @Parameters(name = "{0}")
  def data(): java.util.Collection[Array[Any]] = {
    val testCases = for {
      testCase <- ScalaTestCaseSource.testCases()
      name = testCase.getFileName.toString
      code = readContent(testCase)
      scalacOutput = Scalac().compile(code).mapValues(convert)
      dottyOutput = Dotty().compile(code).mapValues(convert)
    } yield Array[Any](name, scalacOutput, dottyOutput)

    testCases.asJavaCollection
  }

  private def readContent(path: Path): String = new String(Files.readAllBytes(path))

  private def convert(binary: BinaryTasty): TastyADT = {
    val input = new BinaryInput(binary.bytes)
    TastyADTUnpickler.unpickle(input)
  }
}
