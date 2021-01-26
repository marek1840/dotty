package tasty.binary

trait TaggedSectionUnpickler[A] {
  protected def startsSubsection(tag: Int): Boolean

  final def unpickle(input: BinaryInput): A = {
    val tag = input.readByte()
    if (startsSubsection(tag)) unpickle(tag)(input.subsection())
    else unpickle(tag)(input)
  }

  def unpickle(tag: Int)(input: BinaryInput): A
}
