package tasty.names

import dotty.tools.dotc.core.tasty.TastyFormat

import scala.tools.nsc.Global


sealed trait TastyName {
  def kind: Int
}

object TastyName {

  case class UTF8(value: String) extends TastyName {
    override def kind: Int = TastyFormat.NameTags.UTF8
  }

  case class Qualified(qualifier: TastyName, name: TastyName) extends TastyName {
    override def kind: Int = TastyFormat.NameTags.QUALIFIED
  }

  case class Expanded(qualifier: TastyName, name: TastyName) extends TastyName {
    override def kind: Int = TastyFormat.NameTags.EXPANDED
  }

  case class ExpandPrefix(qualifier: TastyName, name: TastyName) extends TastyName {
    override def kind: Int = TastyFormat.NameTags.EXPANDPREFIX
  }

  case class Unique(separator: String, id: Int, underlying: Option[TastyName]) extends TastyName {
    override def kind: Int = TastyFormat.NameTags.UNIQUE
  }

  case class DefaultGetter(underlying: TastyName, id: Int) extends TastyName {
    override def kind: Int = TastyFormat.NameTags.DEFAULTGETTER
  }

  case class Variance(underlying: TastyName, variance: Int) extends TastyName {
    override def kind: Int = TastyFormat.NameTags.VARIANT
  }

  case class SuperAccessor(underlying: TastyName) extends TastyName {
    override def kind: Int = TastyFormat.NameTags.SUPERACCESSOR
  }

  case class InlineAccessor(underlying: TastyName) extends TastyName {
    override def kind: Int = TastyFormat.NameTags.INLINEACCESSOR
  }

  case class ObjectClass(underlying: TastyName) extends TastyName {
    override def kind: Int = TastyFormat.NameTags.OBJECTCLASS
  }

  case class Signed(original: TastyName, parameters: Seq[TastyName], result: TastyName) extends TastyName {
    override def kind: Int = TastyFormat.NameTags.SIGNED
  }

}

trait ScalacNameConversions {

  implicit def nameToScalacName(name: Global#Name): TastyName = {
    nameToScalacName(name.toString)
  }

  implicit def nameToScalacName(names: Seq[Global#Name]): Seq[TastyName] = {
    names.map(nameToScalacName)
  }

  implicit def nameToScalacNameStr(names: Seq[String]): Seq[TastyName] = {
    names.map(nameToScalacName)
  }

  implicit def nameToScalacName(name: String): TastyName = {
    val stringName = name.toString
    if (stringName.contains('.')) {
      val (prefix, qualifier) = splitAtLastChar(stringName, '.')
      TastyName.Qualified(nameToScalacName(prefix), nameToScalacName(qualifier))
    } else {
      TastyName.UTF8(stringName)
    }
  }

  private def splitAtLastChar(str: String, char: Char): (String, String) = {
    val (prefix, qual) = str.splitAt(str.lastIndexOf(char))
    (prefix, qual.drop(1))
  }
}