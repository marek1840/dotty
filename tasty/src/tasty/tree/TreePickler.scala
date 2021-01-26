package tasty.tree

import dotty.tools.dotc.core.tasty.TastyFormat._
import tasty.binary.SectionPickler
import tasty.names.PicklerNamePool
import tasty.tree.types.{ConstantPickler, TypePickler}

import scala.collection.mutable

abstract class TreePickler[Tree, Name](nameSection: PicklerNamePool[Name],
                                       underlying: SectionPickler)
  extends TreeSectionPickler[Tree, Name](nameSection, underlying) {

  protected type Type
  protected type Modifier
  protected type Constant

  final val cache = mutable.Map[Tree, Int]()

  protected def typePickler: TypePickler[Type, Name]

  protected def constantPickler: ConstantPickler[Constant, Name]

  protected def modifierPickler: ModifierPickler[Modifier, Name]

  final def pickle(value: Tree): Unit =
    if (cache.contains(value)) tagged(SHAREDterm) {
      pickleRef(cache(value))
    } else {
      cache += value -> currentOffset
      dispatch(value)
    }

  protected def dispatch(tree: Tree): Unit

  // Top Level Statements
  protected final def picklePackageDef(id: Tree, statements: Seq[Tree]): Unit = tagged(PACKAGE) {
    pickle(id)
    pickleSequence(statements)
  }

  protected final def pickleTypeDef(name: Name, template: Tree, modifiers: Seq[Modifier]): Unit = tagged(TYPEDEF) {
    pickleName(name)
    pickle(template)
    modifierPickler.pickleSequence(modifiers)
  }

  protected final def pickleTemplate(typeParameters: Seq[Any], parameters: Seq[Any], parents: Seq[Tree],
                                    self: Option[(Name, Tree)], statements: Seq[Tree]): Unit = tagged(TEMPLATE) {
    // TODO {type,}parameters

    pickleSequence(parents)
    self.foreach {
      case (name, tp) =>
        pickleName(name)
        pickle(tp)
    }
    pickleSequence(statements)
  }

  protected def pickleDefDef(name: Name, typeParameters: Seq[Any], curriedParams: Seq[Seq[Tree]],
                            returnType: Type, body: Option[Tree], modifiers: Seq[Modifier]): Unit = tagged(DEFDEF) {
    pickleName(name)
    // TODO type parameters
    curriedParams.foreach { parameters =>
      tagged(PARAMS) {
        pickleSequence(parameters)
      }
    }
    typePickler.pickle(returnType)
    body.foreach(pickle)
    modifierPickler.pickleSequence(modifiers)
  }

  // Terms
  protected final def pickleIdent(name: Name, typ: Type): Unit = tagged(IDENT) {
    pickleName(name)
    typePickler.pickle(typ)
  }

  protected final def pickleSelect(name: Name, term: Tree): Unit = tagged(TERMREF) {
    pickleName(name)
    pickle(term)
  }

  protected final def picklePackageRef(name: Name): Unit = tagged(TERMREFpkg)(pickleName(name))

  protected final def pickleBlock(expression: Tree, statements: Seq[Tree]): Unit = tagged(BLOCK) {
    pickle(expression)
    pickleSequence(statements)
  }

  protected final def pickleNew(typ: Type): Unit = tagged(NEW) {
    typePickler.pickle(typ)
  }

  protected final def pickleApply(function: Tree, args: Seq[Tree]): Unit = tagged(APPLY) {
    pickle(function)
    pickleSequence(args)
  }

  protected final def pickleTypeApply(function: Tree, args: Seq[Tree]): Unit = tagged(TYPEAPPLY) {
    pickle(function)
    pickleSequence(args)
  }

  protected final def pickleSuper(term: Tree, mixin: Option[Tree]): Unit = tagged(SUPER) {
    pickle(term)
    mixin.foreach(pickle)
  }
}
