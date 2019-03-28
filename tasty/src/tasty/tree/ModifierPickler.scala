package tasty.tree

import dotty.tools.dotc.core.tasty.TastyFormat._
import tasty.Pickler
import tasty.binary.SectionPickler
import tasty.names.PicklerNamePool
import tasty.tree.ModifierPickler.QualifiedScope

abstract class ModifierPickler[Modifier, Name](nameSection: PicklerNamePool[Name],
                                               underlying: SectionPickler)
  extends TreeSectionPickler[Modifier, Name](nameSection, underlying) {

  protected type Type
  protected type Tree

  protected val typePickler: Pickler[Type]
  protected val treePickler: Pickler[Tree]

  protected final def pickleQualifiedScope(scope: QualifiedScope, t: Type): Unit = tagged(scope.tag) {
    typePickler.pickle(t)
  }

  protected final def pickleModifier(modifier: ModifierPickler.Modifier): Unit = pickleByte(modifier.tag)

  protected final def pickleAnnotation(typ: Type, tree: Tree): Unit = tagged(ANNOTATION) {
    typePickler.pickle(typ)
    treePickler.pickle(tree)
  }
}

object ModifierPickler {

  case class QualifiedScope private(tag: Int)

  val QualifiedPrivate = QualifiedScope(PRIVATEqualified)
  val QualifiedProtected = QualifiedScope(PROTECTEDqualified)

  case class Modifier private(tag: Int)

  val Private = Modifier(PRIVATE)
  val Internal = Modifier(INTERNAL)
  val Protected = Modifier(PROTECTED)

  val Abstract = Modifier(ABSTRACT)
  val Final = Modifier(FINAL)
  val Sealed = Modifier(SEALED)

  val Case = Modifier(CASE)
  val Implicit = Modifier(IMPLICIT)
  val Implied = Modifier(IMPLIED)
  val Erased = Modifier(ERASED)
  val Lazy = Modifier(LAZY)
  val Override = Modifier(OVERRIDE)
  val Opaque = Modifier(OPAQUE)
  val Inline = Modifier(INLINE)
  val Macro = Modifier(MACRO)
  val InlineProxy = Modifier(INLINEPROXY)
  val Static = Modifier(STATIC)
  val Object = Modifier(OBJECT)
  val Trait = Modifier(TRAIT)
  val Enum = Modifier(ENUM)
  val Local = Modifier(LOCAL)
  val Synthetic = Modifier(SYNTHETIC)
  val Artifact = Modifier(ARTIFACT)
  val Mutable = Modifier(MUTABLE)
  val FieldAccessor = Modifier(FIELDaccessor)
  val CaseAccessor = Modifier(CASEaccessor)
  val Covariant = Modifier(COVARIANT)
  val Contravariant = Modifier(CONTRAVARIANT)
  val Scala2x = Modifier(SCALA2X)
  val ParametrizedDefault = Modifier(DEFAULTparameterized)
  val Stable = Modifier(STABLE)
  val Extension = Modifier(EXTENSION)
  val Given = Modifier(GIVEN)
  val SetterParameter = Modifier(PARAMsetter)
}