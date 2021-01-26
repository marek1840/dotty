package tasty.names

import tasty.names.TastyName._

object TastyNameSectionUnpickler extends NameSectionUnpickler[TastyName] {
  override protected def createUnpickler(names: Seq[TastyName]): NameUnpickler[TastyName] = new TastyNameUnpickler(names)

  private final class TastyNameUnpickler(names: Seq[TastyName]) extends NameUnpickler[TastyName](names) {
    override protected def unpickleUTF8(value: String): TastyName =
      UTF8(value)

    override protected def unpickleQualified(qualifier: TastyName, name: TastyName): TastyName =
      Qualified(qualifier, name)

    override protected def unpickleExpanded(qualifier: TastyName, name: TastyName): TastyName =
      Expanded(qualifier, name)

    override protected def unpickleExpandPrefix(qualifier: TastyName, name: TastyName): TastyName =
      ExpandPrefix(qualifier, name)

    override protected def unpickleUnique(separator: TastyName, id: Int, underlying: Option[TastyName]): TastyName =
      Unique(separator.toString, id, underlying)

    override protected def unpickleDefaultGetter(underlying: TastyName, index: Int): TastyName =
      DefaultGetter(underlying, index)

    override protected def unpickleVariant(underlying: TastyName, variance: Int): TastyName =
      Variance(underlying, variance)

    override protected def unpickleSuperAccessor(underlying: TastyName): TastyName =
      SuperAccessor(underlying)

    override protected def unpickleInlineAccessor(underlying: TastyName): TastyName =
      InlineAccessor(underlying)

    override protected def unpickleObjectClass(underlying: TastyName): TastyName =
      ObjectClass(underlying)

    override protected def unpickleSigned(original: TastyName, result: TastyName, parameters: Seq[TastyName]): TastyName =
      Signed(original, parameters, result)
  }

}
