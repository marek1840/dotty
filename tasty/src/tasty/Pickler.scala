package tasty

trait Pickler[-A] {
  def pickle(value: A): Unit

  def pickleSequence(values: Seq[A], includeLength: Boolean = false): Unit
}
