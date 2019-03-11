package tasty4scalac.tasty

import dotty.tools.dotc.core.Names.{EmptyTermName, TermName, Name => DottyName}
import dotty.tools.dotc.core.{NameKinds, Signature, Names => DottyNames}

import scala.collection.mutable

final class DottyNameBuilder extends TastyNameBuilder[DottyName] {
  private final val names = mutable.ArrayBuffer[DottyName]()

  override def utf8(value: String): Unit = {
    names += DottyNames.termName(value)
  }

  override def qualified(q: Int, s: Int): Unit = {
    val qualifier = termAt(q)
    val name = termAt(s).toSimpleName
    names += NameKinds.QualifiedName(qualifier, name)
  }

  override def expanded(q: Int, s: Int): Unit = {
    val qualifier = termAt(q)
    val name = termAt(s).toSimpleName
    names += NameKinds.ExpandedName(qualifier, name)
  }

  override def expandedPrefix(q: Int, s: Int): Unit = {
    val qualifier = termAt(q)
    val name = termAt(s).toSimpleName
    names += NameKinds.ExpandPrefixName(qualifier, name)
  }

  override def unique(sep: Int, id: Int, underlying: Option[Int]): Unit = {
    val separator = termAt(sep).toString
    val term = underlying.map(termAt).getOrElse(EmptyTermName)
    names += NameKinds.uniqueNameKindOfSeparator(separator)(term, id.toInt)
  }

  override def defaultGetter(underlying: Int, index: Int): Unit = {
    val term = termAt(underlying)
    names += NameKinds.DefaultGetterName(term, index.toInt)
  }

  override def variant(underlying: Int, variance: Int): Unit = {
    val term = termAt(underlying)
    names += NameKinds.VariantName(term, variance.toInt)
  }

  override def superAccessor(ref: Int): Unit = {
    val term = termAt(ref)
    names += NameKinds.SuperAccessorName(term)
  }

  override def inlineAccessor(ref: Int): Unit = {
    val term = termAt(ref)
    names += NameKinds.InlineAccessorName(term)
  }

  override def objectClass(ref: Int): Unit = {
    val term = termAt(ref)
    names += NameKinds.ModuleClassName(term)
  }

  override def signed(originalRef: Int, resultRef: Int, parameterRefs: Seq[Int]): Unit = {
    val original = termAt(originalRef)
    val result = termAt(resultRef).toTypeName
    val parameters = parameterRefs.map(termAt(_).toTypeName).toList
    val signature = Signature(parameters, result)
    names += NameKinds.SignedName(original, signature)
  }

  private def termAt(offset: Int): TermName = {
    if (names(offset) == null) throw new UnsupportedOperationException(s"Forward reference unsupported. Referenced term at $offset")
    else names(offset).asTermName
  }

  def build(): Seq[DottyName] = names
}