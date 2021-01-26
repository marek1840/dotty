package tasty.names

import dotty.tools.dotc.core.tasty.TastyFormat.NameTags
import dotty.tools.dotc.core.tasty.TastyFormat.NameTags.{QUALIFIED, UTF8}
import tasty.binary.SectionPickler

import scala.collection.mutable

abstract class PicklerNamePool[Name](output: SectionPickler) {

  private val pool = mutable.Map[Name, NameRef]()

  protected def pickle(name: Name): Unit


  final def pickleName(name: Name): NameRef = {
    pool.getOrElseUpdate(name, insert(name))
  }

  protected def pickleQualifiedName(prefix: NameRef, name: NameRef): Unit = tagged(NameTags.QUALIFIED) {
    pickleNameRef(prefix)
    pickleNameRef(name)
  }

  protected def pickleSignedName(original : NameRef, resultTypeName : NameRef, parameters: Seq[NameRef]): Unit = tagged(NameTags.SIGNED){
    pickleNameRef(original)
    pickleNameRef(resultTypeName)
    parameters.foreach(pickleNameRef)
  }

  private def insert(name: Name): NameRef = {
    pickle(name)
    new NameRef(pool.size)
  }

  protected final def pickleUtf8(bytes: Array[Byte]): Unit = tagged(UTF8) {
    output.pickleBytes(bytes)
  }

  protected final def pickleQualified(qualifier: NameRef, name: NameRef): Unit = tagged(QUALIFIED) {
    pickleNameRef(qualifier)
    pickleNameRef(name)
  }

  private final def tagged(tag: Int)(op: => Unit): Unit = {
    output.pickleByte(tag)
    output.pickleSubsection(op)
  }

  private def pickleNameRef(ref: NameRef): Unit = output.pickleNat(ref.index)
}