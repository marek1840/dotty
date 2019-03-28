package tasty.tree.types

import dotty.tools.dotc.core.tasty.TastyFormat.{TERMREFpkg, TYPEREFpkg}
import tasty.Pickler
import tasty.binary.SectionPickler
import tasty.names.{TastyName, ScalacNameConversions, ScalacPicklerNamePool}

import scala.tools.nsc.Global

final class ScalacTypePickler(nameSection: ScalacPicklerNamePool,
                              underlying: SectionPickler)
                             (implicit g: Global)
  extends TypePickler[Global#Type, TastyName](nameSection, underlying) with ScalacNameConversions {

  override type Constant = Global#Constant
  override protected val constantPickler: Pickler[Constant] = new ScalacConstantPickler(nameSection, underlying)

  override protected def dispatch(t: Global#Type): Unit = t match {
    case g.ConstantType(constant) => constantPickler.pickle(constant)
    case g.SingleType(pre, sym) =>
      if (sym.hasPackageFlag) tagged(if (sym.isType) TYPEREFpkg else TERMREFpkg) {
        pickleName(sym.fullNameAsName('.'))
      } else {
        ???
      }
    case g.TypeRef(pre, sym, args) =>
      if (args.isEmpty) {
        val name = sym.name
        pickleTypeRef(name, pre)
      } else ??? // TODO APPLIEDTYPE

    case tpe@g.ThisType(sym) =>
      if (sym.isRoot || !sym.hasPackageFlag) {
        val typeConstructor = tpe.underlying.typeConstructor
        pickleThis(typeConstructor)
      } else {
        picklePackageTermRef(sym.fullNameAsName('.'))
      }

    case g.TypeBounds(lo, hi) => pickleTypeBounds(lo, hi)


    case _ => throw new UnsupportedOperationException(s"Cannot pickle type [${t.getClass} $t]")
  }
}
