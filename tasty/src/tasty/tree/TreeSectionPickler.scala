package tasty.tree

import dotty.tools.dotc.core.tasty.TastyFormat
import tasty.TastySectionPickler
import tasty.binary.SectionPickler
import tasty.names.PicklerNamePool

protected[tree] abstract class TreeSectionPickler[A, Name](nameSection: PicklerNamePool[Name], output: SectionPickler)
  extends TastySectionPickler[A, Name](nameSection, output) {

  protected final def tagged(tag: Int)(op: => Unit): Unit = {
    output.pickleByte(tag)
    if (tag >= TastyFormat.firstLengthTreeTag) pickleSubsection(op)
    else op
  }

  protected final def pickleRef(value: Int): Unit = output.pickleNat(value)
}
