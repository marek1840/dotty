package tasty4scalac.tasty

trait TastyNameBuilder[T] {
  def build(): Seq[T]

  def utf8(value: String): Unit

  def qualified(qualRef: Int, sepRef: Int): Unit

  def expanded(qualRef: Int, sepRef: Int): Unit

  def expandedPrefix(qualRef: Int, sepRef: Int): Unit

  def unique(sep: Int, id: Int, underlying: Option[Int]): Unit

  def defaultGetter(underlying: Int, index: Int): Unit

  def variant(underlying: Int, variance: Int): Unit

  def superAccessor(ref: Int): Unit

  def inlineAccessor(ref: Int): Unit

  def objectClass(ref: Int): Unit

  def signed(originalRef: Int, resultRef: Int, parameters: Seq[Int]): Unit
}
