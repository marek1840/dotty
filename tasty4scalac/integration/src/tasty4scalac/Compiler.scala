package tasty4scalac


trait Compiler {
  def compile(code: String): Set[BinaryTasty]

  final override def toString: String = getClass.getSimpleName
}

object Compiler {
  def dotty(): Compiler = Dotty()
  def scalac(): Compiler = Scalac()
}
