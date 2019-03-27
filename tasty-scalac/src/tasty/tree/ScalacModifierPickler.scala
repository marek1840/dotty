package tasty.tree

import tasty.Pickler
import tasty.binary.SectionPickler
import tasty.names.ScalacPicklerNamePool
import tasty.tree.types.ScalacTypePickler

import scala.tools.nsc.Global

final class ScalacModifierPickler(nameSection: ScalacPicklerNamePool,
                                  underlying: SectionPickler)
                                 (implicit val g: Global)
  extends ModifierPickler[Global#Symbol, Global#Name](nameSection, underlying) {

  import ModifierPickler._

  override protected type Type = Global#Type


  override protected val typePickler: Pickler[Global#Type] = new ScalacTypePickler(nameSection, underlying)

  override def pickle(symbol: Global#Symbol): Unit = {
    import symbol._

    if (privateWithin.exists) {
      val scope = if (isProtected) ModifierPickler.QualifiedProtected else ModifierPickler.QualifiedPrivate
      pickleQualifiedScope(scope, privateWithin.toType)
    }
    // TODO inline, static, local

    if (isPrivate || isTypeParameter) pickleModifier(Private)
    if (isProtected && !privateWithin.exists) pickleModifier(Protected)
    if (isModule) pickleModifier(Object)
    if (isFinal && !isModule) pickleModifier(Final)

    if (isCase) pickleModifier(Case)
    if (isOverride) pickleModifier(Override)
    if (isMacro) pickleModifier(Macro)

    if (isTypeParameter) pickleModifier(Local)
    if (isSynthetic) pickleModifier(Synthetic)
    if (isArtifact) pickleModifier(Artifact)

    //    pickleModifier(SCALA2X) // TODO ?

    val moduleClass = false // TODO
    if (isTerm && !moduleClass) {
      if (isImplicit) pickleModifier(Implicit)
      //             if (symbol is Erased) pickleModifier(ERASED)
      if (isLazy && !isModule) pickleModifier(Lazy)
      if (hasFlag(scala.reflect.internal.Flags.ABSOVERRIDE)) {
        pickleModifier(Abstract)
        pickleModifier(Override)
      }
      val mutable = false // TODO
      val field = false // TODO
      val methodParam = false // TODO
      if (isMutable || isSetter || mutable) pickleModifier(Mutable)
      if (isAccessor && !field) pickleModifier(FieldAccessor)
      if (isCaseAccessor) pickleModifier(CaseAccessor)
      // if (isDefaultParameterized) pickleModifier(DEFAULTparameterized) // TODO ?
      if (isStable && !methodParam) pickleModifier(Stable)
      if (isParamAccessor && isSetter) pickleModifier(SetterParameter)
    } else {
      if (isSealed) pickleModifier(Sealed)
      if (isAbstract && !isTypeParameter) pickleModifier(Abstract)
      if (isTrait) pickleModifier(Trait)
      if (isCovariant) pickleModifier(Covariant)
      if (isContravariant) pickleModifier(Contravariant)
    }
  }
}
