package tasty.tree.terms

import dotty.tools.dotc.core.tasty.TastyFormat.SELECT
import tasty.ScalacConversions
import tasty.binary.SectionPickler
import tasty.names.{TastyName, ScalacNameConversions, ScalacPicklerNamePool}
import tasty.tree.types.{ScalacConstantPickler, ScalacTypePickler}
import tasty.tree.{ScalacModifierPickler, TreePickler}

import scala.tools.nsc.Global

final class ScalacTreePickler(nameSection: ScalacPicklerNamePool,
                              underlying: SectionPickler)
                             (implicit val g: Global)
  extends TreePickler[Global#Tree, TastyName](nameSection, underlying) with ScalacConversions with ScalacNameConversions {

  override protected type Type = Global#Type
  override protected type Modifier = Global#Symbol
  override protected type Constant = Global#Constant

  override protected val typePickler: ScalacTypePickler = new ScalacTypePickler(nameSection, underlying)

  override protected val constantPickler: ScalacConstantPickler = new ScalacConstantPickler(nameSection, underlying)

  override protected val modifierPickler: ScalacModifierPickler = new ScalacModifierPickler(nameSection, underlying)

  override protected def dispatch(tree: Global#Tree): Unit = if (tree.nonEmpty) {
    val symbol = tree.symbol
    // symbol might be null
    lazy val owner = symbol.owner

    tree match {
      case g.PackageDef(id, statements) => picklePackageDef(id, statements)
      case g.ClassDef(mods, name, tparams, impl) => pickleTypeDef(name, impl, Seq(tree.symbol))
      case g.Template(parents, self, body) =>
        // TODO type parameters and parameters and self

        // need to pickle the super constructor call as parent_term
        val parentConstructor = body.find(_.symbol.isPrimaryConstructor).map {
          case defdef: g.DefDef => defdef.rhs.asInstanceOf[Global#Block].stats.head
        }

        pickleTemplate(Nil, Nil, parentConstructor.toSeq, None, body)

      case tree@g.DefDef(mods, name, tparams, vparams, tpt, rhs) =>
        val returnType = if (tree.symbol.isConstructor) g.TypeTree(g.definitions.UnitTpe) else tpt
        val body = if (tree.symbol.isPrimaryConstructor) None // TODO: Check if there's no information lost here
        else Some(tree.rhs)
        val name = if (symbol.isConstructor && owner.isTrait) g.nme.CONSTRUCTOR // FIXME: this is not enough, if trait is PureInterface, no $init$ is generated at all
        else symbol.name

        pickleDefDef(name, tparams, vparams, returnType.tpe, body, Seq(symbol))

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
        } else if (symbol.isMethod) tree.tpe match {
          case g.MethodType(params, resultType) =>
            // TODO this doesn't deal with changing scala.Any to Object
            val paramTypeNames = params.map(_.typeOfThis.typeSymbol.fullName)
            val resultTypeName = resultType.typeSymbol.fullName
            if (symbol.isConstructor) {
              val signedName = TastyName.Signed("<init>", paramTypeNames, resultTypeName)
              pickleConstructor(signedName, resultType)
            } else {
              val signedName = TastyName.Signed(name, paramTypeNames, resultTypeName)
              pickleSelect(signedName, qualifier)
            }
        } else pickleSelect(name, qualifier)

      case g.Apply(fun, args) => pickleApply(fun, args)

      case g.Block(stats, expr) => pickleBlock(expr, stats)

      case g.TypeTree() => typePickler.pickle(tree.tpe)

      case g.Literal(constant) => constantPickler.pickle(constant)

      case _ => throw new UnsupportedOperationException(s"Cannot pickle tree: [${tree.getClass}: $tree")
    }
  }

  private def pickleConstructor(initName: TastyName, tpe: Global#Type): Unit = tagged(SELECT) {
    pickleName(initName)
    pickleNew(tpe)
  }
}
