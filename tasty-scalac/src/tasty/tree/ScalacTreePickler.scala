package tasty.tree.terms

import dotty.tools.dotc.core.tasty.TastyFormat.SELECT
import tasty.ScalacConversions
import tasty.binary.SectionPickler
import tasty.names.ScalacPicklerNamePool
import tasty.tree.types.{ScalacConstantPickler, ScalacTypePickler}
import tasty.tree.{ScalacModifierPickler, TreePickler}

import scala.tools.nsc.Global

final class ScalacTreePickler(nameSection: ScalacPicklerNamePool,
                             underlying: SectionPickler)
                            (implicit val g: Global)
  extends TreePickler[Global#Tree, Global#Name](nameSection, underlying) with ScalacConversions {

  override protected type Type = Global#Type
  override protected type Modifiers = Global#Symbol
  override protected type Constant = Global#Constant

  override protected val typePickler: ScalacTypePickler = new ScalacTypePickler(nameSection, underlying)

  override protected val constantPickler: ScalacConstantPickler = new ScalacConstantPickler(nameSection, underlying)

  override protected val modifierPickler: ScalacModifierPickler = new ScalacModifierPickler(nameSection, underlying)

  override protected def dispatch(tree: Global#Tree): Unit = {
    val symbol = tree.symbol
    // symbol might be null
    lazy val owner = symbol.owner

    tree match {
      case g.PackageDef(id, statements) => picklePackageDef(id, statements)
      case g.TypeDef(mods, name, tparams, rhs) =>
        assert(tparams.isEmpty, "Higher kinded types are not yet supported") // TODO
        if (symbol.isTypeParameter) pickleTypeParameter(name, rhs, symbol)
        else pickleTypeDef(name, rhs, symbol)

      case g.ClassDef(mods, name, tparams, impl) =>
        val template = impl.copy(body = tparams ::: impl.body) // scalac does not include type parameters in templates
        pickleTypeDef(name, template, symbol)

      case g.Template(parents, self, body) =>
        val (typeParameters, members) = body partition {
          case stat: g.TypeDef => stat.symbol.isParameter
          case stat: g.ValOrDefDef =>
            stat.symbol.isParamAccessor && !stat.symbol.isSetter && !stat.symbol.isAccessor
          case _ => false
        }

        // TODO check if it could be sorted instead of partitioned? constructors must be first
        val (constructors, nonConstructor) = members partition {
          case stat: g.DefDef => stat.symbol.isConstructor
          case _ => false
        }

        // need to pickle the super constructor call as parent_term
        val parentConstructor = body.find(_.symbol.isPrimaryConstructor).map {
            case defdef: g.DefDef => defdef.rhs.asInstanceOf[Global#Block].stats.head
        }

        pickleTemplate(typeParameters, Nil, parentConstructor.toSeq, None, constructors ::: nonConstructor)

      case tree@g.DefDef(mods, name, tparams, vparams, tpt, rhs) =>
        val returnType = if (symbol.isConstructor) g.TypeTree(g.definitions.UnitTpe) else tpt
        val body = if (symbol.isPrimaryConstructor) None // TODO: Check if there's no information lost here
        else Some(tree.rhs)
        val name = if (symbol.isConstructor && owner.isTrait) g.nme.CONSTRUCTOR // FIXME: this is not enough, if trait is PureInterface, no $init$ is generated at all
        else symbol.name

        val typeParameters = if (symbol.isConstructor) { // scalac does not include tpyeparameters in constructors
          symbol.owner.info.typeParams.map { paramSymbol =>
            val tree = new g.TypeTree().setType(paramSymbol.info)
            g.newTypeDef(paramSymbol, tree)() // FIXME this is pickled as TYPEBOUNDS, while dotty is pickling TYPEBOUNDStpt
          }
        } else tparams

        pickleDefDef(name, typeParameters, vparams, returnType, body, symbol)

      case g.Ident(name) =>
        val isNonWildcardTerm = tree.isTerm && name != g.nme.WILDCARD
        if (isNonWildcardTerm) {
          // The type of a tree Ident should be a TERMREF
          val tp1 = tree.tpe match {
            case _: g.TypeRef => g.SingleType(owner.thisType, symbol)
            case _: g.MethodType => g.SingleType(owner.thisType, symbol) // Happens on calls to LabelDefs
            case t => t
          }
          typePickler.pickle(tp1)
        }
        else if (tree.isTerm) pickleIdent(name, tree.tpe)
        else ??? // TODO pickleIdentTpt(name, tree.tpe)

      case g.Select(qualifier, name) =>
        val appliesTypesToConstructor = symbol.isConstructor && owner.typeParams.nonEmpty

        if (appliesTypesToConstructor) {
          val g.TypeRef(_, _, targs) = qualifier.tpe.widen
          ??? // TODO pickleTypeApply(tree, targs)
        } else if (symbol.hasPackageFlag && !symbol.isRoot) {
          picklePackageRef(g.TermName(tree.toString()))
        } else if (symbol.isConstructor) {
          pickleConstructor(g.TermName("<init>"), owner.typeOfThis)
        } else pickleSelect(name, qualifier)

      case g.Apply(fun, args) => pickleApply(fun, args)

      case g.Block(stats, expr) => pickleBlock(expr, stats)

      case g.TypeTree() => typePickler.pickle(tree.tpe)

      case g.Literal(constant) => constantPickler.pickle(constant)

      case _ => throw new UnsupportedOperationException(s"Cannot pickle tree: [${tree.getClass}: $tree")
    }
  }

  private def pickleConstructor(initName: Global#Name, tpe: Global#Type): Unit = tagged(SELECT) {
    pickleName(initName)
    pickleNew(tpe)
  }
}
