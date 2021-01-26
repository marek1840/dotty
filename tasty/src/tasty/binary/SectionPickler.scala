package tasty.binary

import dotty.tools.dotc.core.tasty.TastyBuffer
import tasty.Pickler

class SectionPickler extends BinaryPickler {
  final def pickle[A](value: A)(implicit pickler: Pickler[A]): Unit = pickler.pickle(value)

  final def pickleSequence[A](sequence: Seq[A], includeLength: Boolean = false)(implicit pickler: Pickler[A]): Unit =
    if (includeLength) pickleSubsection(pickleSequence(sequence))
    else sequence.foreach(pickle(_))

  final def pickleSubsection(op: => Unit): Unit = {
    val start = length
    val subsectionOffset = start + TastyBuffer.AddrWidth

    // write subsection
    length = subsectionOffset
    op
    val subsectionLength = length - subsectionOffset

    // write subsection length
    length = start
    pickleNat(subsectionLength)
    val padding = subsectionOffset - length

    // strip zeros
    // TODO given N nested sections it will move bytes N times. Maybe compact when growing?
    // TODO still needs an index of first padding
    if (padding > 0) {
      // leaves some trash after the length - but it should be of no concern
      System.arraycopy(buffer, subsectionOffset, buffer, length, subsectionLength)
    }

    // set pointer after the subsection
    length += subsectionLength
  }
}