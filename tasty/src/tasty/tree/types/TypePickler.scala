package tasty.tree.types

import dotty.tools.dotc.core.tasty.TastyFormat._
import tasty.Pickler
import tasty.binary.SectionPickler
import tasty.names.PicklerNamePool
import tasty.tree.TreeSectionPickler

import scala.collection.mutable

abstract class TypePickler[Type, Name](nameSection: PicklerNamePool[Name],
                                       underlying: SectionPickler)
  extends TreeSectionPickler[Type, Name](nameSection, underlying) {
  type Constant

  final val cache = mutable.Map[Type, Int]()

  protected def constantPickler: Pickler[Constant]

  final def pickle(value: Type): Unit =
    if (cache.contains(value)) tagged(SHAREDtype) {
      pickleRef(cache(value))
    } else {
      cache += value -> currentOffset
      dispatch(value)
    }

  protected def dispatch(value: Type): Unit

  protected final def pickleTypeRef(name: Name, pre: Type): Unit = tagged(TYPEREF) {
    pickleName(name)
    pickle(pre)
  }

  protected final def pickleThis(typeConstructor: Type): Unit = tagged(THIS)(pickle(typeConstructor))

  protected final def picklePackageTermRef(name: Name): Unit = tagged(TERMREFpkg) {
    pickleName(name)
  }

  protected final def pickleAnd(left: Type, right: Type): Unit = tagged(ANDtype) {
    pickle(left)
    pickle(right)
  }

  protected final def pickleOr(left: Type, right: Type): Unit = tagged(ORtype) {
    pickle(left)
    pickle(right)
  }
}
