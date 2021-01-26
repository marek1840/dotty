package tasty

import tasty.binary.SectionPickler
import tasty.names.PicklerNamePool

abstract class TastySectionPickler[A, Name](nameSection: PicklerNamePool[Name], underlying: SectionPickler) extends Pickler[A] {
  override final def pickleSequence(sequence: Seq[A], includeLength: Boolean = false): Unit =
    underlying.pickleSequence(sequence, includeLength)(pickler = this)

  protected final def currentOffset: Int = underlying.size

  protected final def pickleName(name: Name): Unit = {
    val ref = nameSection.pickleName(name)
    underlying.pickleNat(ref.index)
  }

  protected final def pickleSubsection(op: => Unit): Unit =
    underlying.pickleSubsection(op)

  protected final def pickleNat(value: Int): Unit =
    underlying.pickleNat(value)

  protected final def pickleInteger(value: Int): Unit =
    underlying.pickleInteger(value)

  protected final def pickleLong(value: Long): Unit =
    underlying.pickleLong(value)
}