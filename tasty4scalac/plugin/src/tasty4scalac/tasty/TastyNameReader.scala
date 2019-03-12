package tasty4scalac.tasty

import dotty.tools.dotc.core.tasty.TastyBuffer.Addr
import dotty.tools.dotc.core.tasty.TastyFormat.NameTags._
import dotty.tools.dotc.core.tasty.{TastyReader => Reader}

import scala.annotation.tailrec

object TastyNameReader {
  def apply[T](source: Reader, builder: TastyNameBuilder[T]): TastyNameReader[T] = {
    val bytes = source.readNat()
    val input = source.subReader(source.currentAddr, source.currentAddr + bytes)
    new TastyNameReader[T](input, builder)
  }
}

final class TastyNameReader[T] private(input: Reader, builder: TastyNameBuilder[T]) {
  @tailrec
  def readNames(): Seq[T] =
    if (input.isAtEnd) builder.build()
    else {
      val tag = input.readByte()
      val length = input.readNat()
      readName(tag, length, end = input.currentAddr + length)
      readNames()
    }

  private def readName(tag: Int, length: Int, end: Addr): Unit = tag match {
    case UTF8 => builder.utf8(new String(input.readBytes(length)))
    case QUALIFIED => builder.qualified(input.readNat(), input.readNat())
    case EXPANDED => builder.expanded(input.readNat(), input.readNat())
    case EXPANDPREFIX => builder.expandedPrefix(input.readNat(), input.readNat())

    case UNIQUE =>
      val separator = input.readNat()
      val id = input.readNat()
      val original = input.ifBefore(end)(Some(input.readNat()), None)
      builder.unique(separator, id, original)

    case DEFAULTGETTER => builder.defaultGetter(input.readNat(), input.readNat())
    case VARIANT => builder.variant(input.readNat(), input.readNat())

    case SUPERACCESSOR => builder.superAccessor(input.readNat())
    case INLINEACCESSOR => builder.inlineAccessor(input.readNat())
    case OBJECTCLASS => builder.objectClass(input.readNat())

    case SIGNED =>
      val original = input.readNat()
      val result = input.readNat()
      val parameters = input.until(end)(input.readNat())
      builder.signed(original, result, parameters)

    case _ => throw new UnsupportedOperationException(s"Unsupported name tag: $tag")
  }

}

