package tasty4scalac

import dotty.tools.dotc.core.Contexts
import dotty.tools.dotc.core.tasty.TastyPrinter
import tasty.Tasty


case class BinaryTasty(bytes: Array[Byte]) extends Tasty{

  def printContents(implicit ctx : Contexts.Context): Unit ={
    val printer = new TastyPrinter(bytes)
    println(printer.printContents())
  }
}