package tasty

import dotty.tools.dotc.core.Contexts
import tasty.names.TastyName

final class TastyADT(val names: Seq[TastyName]) extends Tasty {
  override def printContents(implicit ctx: Contexts.Context): Unit = {
    names.zipWithIndex.foreach { case (index, name) => println(s"$index: $name") }
  }
}