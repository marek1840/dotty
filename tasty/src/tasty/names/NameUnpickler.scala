package tasty.names

import dotty.tools.dotc.core.NameTags
import dotty.tools.dotc.core.tasty.TastyFormat
import dotty.tools.dotc.core.tasty.TastyFormat.NameTags._
import tasty.binary.{BinaryInput, TaggedSectionUnpickler}

abstract class NameUnpickler[Name](names: Seq[Name]) extends TaggedSectionUnpickler[Name] {

  override protected final def startsSubsection(tag: Int): Boolean = true

  override final def unpickle(tag: Int)(input: BinaryInput): Name = tag match {
    case UTF8 => unpickleUTF8(new String(input.readBytes()))
    case QUALIFIED => unpickleQualified(readName(input), readName(input))
    case EXPANDED => unpickleExpanded(readName(input), readName(input))
    case EXPANDPREFIX => unpickleExpandPrefix(readName(input), readName(input))
    case UNIQUE => unpickleUnique(readName(input), input.readNat(), input.ifNotEmpty(readName))
    case DEFAULTGETTER => unpickleDefaultGetter(readName(input), input.readNat())
    case VARIANT => unpickleVariant(readName(input), input.readNat())
    case SUPERACCESSOR => unpickleSuperAccessor(readName(input))
    case INLINEACCESSOR => unpickleInlineAccessor(readName(input))
    case OBJECTCLASS => unpickleObjectClass(readName(input))
    case SIGNED => unpickleSigned(readName(input), readName(input), input.readSequence(readName))
    case _ => throw new IllegalStateException(s"Unsupported name tag: ${NameTags.nameTagToString(tag)}")
  }

  protected def unpickleUTF8(value: String): Name

  protected def unpickleQualified(qualifier: Name, name: Name): Name

  protected def unpickleExpanded(qualifier: Name, name: Name): Name

  protected def unpickleExpandPrefix(qualifier: Name, name: Name): Name

  protected def unpickleUnique(separator: Name, id: Int, underlying: Option[Name]): Name

  protected def unpickleDefaultGetter(underlying: Name, index: Int): Name

  protected def unpickleVariant(underlying: Name, variance: Int): Name

  protected def unpickleSuperAccessor(underlying: Name): Name

  protected def unpickleInlineAccessor(underlying: Name): Name

  protected def unpickleObjectClass(underlying: Name): Name

  protected def unpickleSigned(original: Name, result: Name, parameters: Seq[Name]): Name




  private def readName(input: BinaryInput): Name = names(input.readNat())
}
