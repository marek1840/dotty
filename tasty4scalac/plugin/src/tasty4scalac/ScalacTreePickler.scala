package tasty4scalac

import dotty.tools._
import dotty.tools.dotc._
import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.NameKinds._
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core._
import dotty.tools.dotc.core.tasty.TastyBuffer._
import dotty.tools.dotc.core.tasty.TastyFormat._
import dotty.tools.dotc.core.tasty.TreeBuffer

import scala.collection.mutable
import scala.tools.nsc.Global

class ScalacTreePickler(pickler: ScalacTastyPickler, val g: Global) {
  val buf = new TreeBuffer
  pickler.newSection("ASTs", buf)
  import buf._
  import pickler.nameBuffer.nameIndex

  private val symRefs = new mutable.HashMap[g.Symbol, Addr]
  private val forwardSymRefs = new mutable.HashMap[g.Symbol, List[Addr]]
  private val pickledTypes = new java.util.IdentityHashMap[g.Type, Any] // Value type is really Addr, but that's not compatible with null

  private def withLength(op: => Unit) = {
    val lengthAddr = reserveRef(relative = true)
    op
    fillRef(lengthAddr, currentAddr, relative = true)
  }

  def pickledSym(sym: g.Symbol): g.Symbol =
    if (sym.isParamAccessor && !sym.isSetter) {
      val getter = sym.getterIn(sym.owner)
      if (getter.exists)
        getter
      else
        sym
    } else
      sym

  def preRegister(tree: g.Tree): Unit = tree match {
    case _: g.MemberDef | _: g.LabelDef | _: g.Function =>
      val sym = pickledSym(tree.symbol)
      if (!symRefs.contains(sym)) symRefs(sym) = NoAddr
    case _ =>
  }

  def registerDef(sym: g.Symbol): Unit = {
    symRefs(sym) = currentAddr
    forwardSymRefs.get(sym) match {
      case Some(refs) =>
        refs.foreach(fillRef(_, currentAddr, relative = false))
        forwardSymRefs -= sym
      case None =>
    }
  }

  private def convertName(name: g.Name): Name =
    if (name eq g.nme.isInstanceOf_Ob)
      nme.isInstanceOf_
    else if (name eq g.nme.asInstanceOf_Ob)
      nme.asInstanceOf_
    else {
      val decoded = name.decode.stripSuffix(" ")
      val parts = decoded.split('.').map(_.toTermName)
      // TODO: unmangle like in Scala2Unpickler#readDisambiguatedSymbol
      val qname = parts.reduce((prefix, part) => QualifiedName(prefix, part.asSimpleName))
      if (name.isTypeName)
        qname.toTypeName
      else
        qname
    }

  private def pickleDottyName(name: Name): Unit = writeNat(nameIndex(name).index)

  private def pickleName(name: g.Name): Unit =
    pickleDottyName(convertName(name))


  private def pickleNameAndSig(name: g.Name, tp: g.Type): Unit = {
    val sig = signature(tp, isConstructor = name eq g.nme.CONSTRUCTOR)
    val name1 = convertName(name)
    pickleDottyName(
      if (sig eq Signature.NotAMethod) name1
      else SignedName(name1.toTermName, sig))
  }

  private def sigName(tp: g.Type): TypeName = tp match {
    case g.TypeRef(_, sym, _) =>
      // TODO: special case arrays, maybe value classes too
      convertName(sym.fullNameAsName('.')).asTypeName
  }

  private def signature(tp: g.Type, isConstructor: Boolean): Signature = tp.erasure match {
    case tp @ g.MethodType(_, resultType0) =>
      val resultType =
        if (isConstructor && (resultType0.typeSymbol eq g.definitions.ArrayClass))
          g.definitions.ObjectTpe // Special case Array constructor, see if we can change Dotty erasure instead
        else
          resultType0
      Signature(tp.paramTypes.map(sigName), sigName(resultType))
    case _ =>
      Signature.NotAMethod
  }

  private def pickleSymRef(sym: g.Symbol) = symRefs.get(sym) match {
    case Some(label) =>
      if (label != NoAddr) writeRef(label) else pickleForwardSymRef(sym)
    case None =>
      assert(false,  s"pickling reference to as yet undefined $sym in ${sym.owner}")
      // pickleForwardSymRef(sym)
  }

  private def pickleForwardSymRef(sym: g.Symbol) = {
    val ref = reserveRef(relative = false)
    assert(!sym.hasPackageFlag, sym)
    forwardSymRefs(sym) = ref :: forwardSymRefs.getOrElse(sym, Nil)
  }

  private def isLocallyDefined(sym: Symbol)(implicit ctx: Context) =
    ??? //sym.topLevelClass.isLinkedWith(pickler.rootCls)

  def pickleConstant(c: g.Constant): Unit = c.tag match {
    case UnitTag =>
      writeByte(UNITconst)
    case BooleanTag =>
      writeByte(if (c.booleanValue) TRUEconst else FALSEconst)
    case ByteTag =>
      writeByte(BYTEconst)
      writeInt(c.byteValue)
    case ShortTag =>
      writeByte(SHORTconst)
      writeInt(c.shortValue)
    case CharTag =>
      writeByte(CHARconst)
      writeNat(c.charValue)
    case IntTag =>
      writeByte(INTconst)
      writeInt(c.intValue)
    case LongTag =>
      writeByte(LONGconst)
      writeLongInt(c.longValue)
    case FloatTag =>
      writeByte(FLOATconst)
      writeInt(java.lang.Float.floatToRawIntBits(c.floatValue))
    case DoubleTag =>
      writeByte(DOUBLEconst)
      writeLongInt(java.lang.Double.doubleToRawLongBits(c.doubleValue))
    case StringTag =>
      writeByte(STRINGconst)
      pickleDottyName(c.stringValue.toTermName)
    case NullTag =>
      writeByte(NULLconst)
    case ClazzTag =>
      writeByte(CLASSconst)
      ??? // pickleType(c.typeValue)
    case EnumTag =>
      writeByte(ENUMconst)
      ??? //pickleType(c.symbolValue.termRef)
    case ScalaSymbolTag =>
      writeByte(SYMBOLconst)
      ??? //pickleName(c.scalaSymbolValue.name.toTermName)
  }

  def pickleType(tpe: g.Type, richTypes: Boolean = false): Unit =
    try {
      val prev = pickledTypes.get(tpe)
      if (prev == null) {
        pickledTypes.put(tpe, currentAddr)
        pickleNewType(tpe, richTypes)
      }
      else {
        writeByte(SHAREDtype)
        writeRef(prev.asInstanceOf[Addr])
      }
    } catch {
      case ex: AssertionError =>
        println(s"error when pickling type $tpe")
        throw ex
    }

  private def pickleNamedType(pre0: g.Type, sym: g.Symbol, isType: Boolean): Unit = {
    val pre =
      if (pre0 == g.NoPrefix && sym.isTypeParameter)
        sym.owner.thisType
      else
        pre0
    if (sym.hasPackageFlag) {
      writeByte(if (isType) TYPEREFpkg else TERMREFpkg)
      pickleName(sym.fullNameAsName('.'))
    }
    else if (sym.isExistential) {
      writeByte(TYPEBOUNDS)
      withLength {
        pickleType(g.definitions.NothingTpe)
        // FIXME: Incorrect for higher-kinded existential variable
        pickleType(g.definitions.AnyTpe)
      }
    }
    else if (pre == g.NoPrefix) {
      writeByte(if (isType) TYPEREFdirect else TERMREFdirect)
      pickleSymRef(sym)
    }
    else {
      writeByte(if (isType) TYPEREF else TERMREF)

      // FIXME do something like this when pickling any name
      val name1 =
        if (sym.isModuleClass)
          sym.name.decode.toTypeName.moduleClassName
        else
          sym.name.decode.toTypeName
      pickleDottyName(name1)
      pickleType(pre)
    }
  }

  private def pickleNewType(tpe: g.Type, richTypes: Boolean): Unit = tpe match {
    case g.ConstantType(value) =>
      pickleConstant(value)
    case g.TypeRef(pre, sym, args) =>
      if (!args.isEmpty) {
        writeByte(APPLIEDtype)
        withLength {
          pickleNamedType(pre, sym, isType = true)
          args.foreach(pickleType(_))
        }
      }
      else
        pickleNamedType(pre, sym, isType = true)
    case g.SingleType(pre, sym) =>
      pickleNamedType(pre, sym, sym.isType)
    case tpe @ g.ThisType(sym) =>
      if (sym.hasPackageFlag && !sym.isRoot) {
        writeByte(TERMREFpkg)
        pickleName(sym.fullNameAsName('.'))
      } else {
        assert(!sym.isRoot)
        writeByte(THIS)
        pickleType(tpe.underlying.typeConstructor.asInstanceOf[g.TypeRef])
      }
    case g.TypeBounds(lo, hi) =>
      if (lo eq hi) {
        writeByte(TYPEALIAS)
        pickleType(lo, richTypes)
      } else {
        writeByte(TYPEBOUNDS)
        withLength { pickleType(lo, richTypes); pickleType(hi, richTypes) }
      }
    case g.AnnotatedType(annotations, underlying) =>
      def pickleAnnot(annots: List[g.AnnotationInfo]): Unit = annots match {
        case a :: as =>
          writeByte(ANNOTATEDtype)
          withLength {
            pickleAnnot(as)
            pickleTree(a.original)
          }
        case Nil =>
          pickleType(underlying, richTypes)
      }
      pickleAnnot(annotations)
    case tpe @ g.ExistentialType(quantified, underlying) =>
      if (!tpe.isRepresentableWithWildcards) {
        assert(false, s"Cannot pickle existential type not representable using wildcards: $tpe")
      }
      pickleType(underlying)
  }

  def pickleTpt(tpt: g.Tree): Unit =
    pickleTree(tpt)

  def pickleTreeUnlessEmpty(tree: g.Tree): Unit =
    if (!tree.isEmpty) pickleTree(tree)

  def pickleDef(tag: Int, sym0: g.Symbol, tpt: g.Tree, rhs0: g.Tree = null, pickleParams: => Unit = ()) = {
    val sym = pickledSym(sym0)
    assert(symRefs(sym) == NoAddr, s"$sym - $sym0")
    registerDef(sym)
    writeByte(tag)
    withLength {
      val name =
        if (sym.isConstructor && sym.owner.isTrait)
          g.nme.CONSTRUCTOR // FIXME: this is not enough, if trait is PureInterface, no $init$ is generated at all
        else
          sym.name
      pickleName(name)
      pickleParams
      tpt match {
        case _: g.Template => pickleTree(tpt)
        case _ if tpt.isType => pickleTree(tpt)
      }
      val rhs =
        if (sym0.isSetter && sym0.isAccessor) {
          if (sym0.isParamAccessor)
            g.EmptyTree
          else
            g.Literal(g.Constant(()))
        }
        else if (!sym.isConstructor && !sym.isDeferred && rhs0 == g.EmptyTree) // account for var x: A = _
                // FIXME: should be && isMutable but var fields in traits are missing Mutable field
                // (it's present on the setter)
          g.Ident(g.nme.WILDCARD).setType(sym.info.resultType)
        else
          rhs0
      if (rhs != null) pickleTreeUnlessEmpty(rhs)
      pickleModifiers(sym,
        mutable = sym0.isMutable, // Mutable flag is on field only
        field = sym0 != sym,
        methodParam = sym0.owner.isMethod && tag == PARAM
      )
    }
  }

  def pickleParam(tree: g.Tree): Unit = {
    registerTreeAddr(tree)
    tree match {
      case tree: g.ValDef => pickleDef(PARAM, tree.symbol, tree.tpt)
      case tree: g.DefDef => pickleDef(PARAM, tree.symbol, tree.tpt, tree.rhs)
      case tree: g.TypeDef => pickleDef(TYPEPARAM, tree.symbol, tree.rhs)
    }
  }

  def pickleParams(trees: List[g.Tree]): Unit = {
    trees.foreach(preRegister)
    trees.foreach(pickleParam)
  }

  def pickleStats(stats: List[g.Tree]) = {
    stats.foreach(preRegister)
    stats.foreach(stat => if (!stat.isEmpty) pickleTree(stat))
  }

  def pickleTree(tree: g.Tree): Unit = {
    // Accessors that are not in traits should not be pickled, they will be reconstructed
    if (tree.isDef && tree.symbol != null && !tree.symbol.owner.isTrait && tree.symbol.isAccessor && !tree.symbol.isSetter) {
      return
    }

    val addr = registerTreeAddr(tree)
    if (addr != currentAddr) {
      writeByte(SHAREDterm)
      writeRef(addr)
    }
    else
      try tree match {
        case g.PackageDef(pid, stats) =>
          writeByte(PACKAGE)
          withLength { pickleType(pid.tpe); pickleStats(stats) }
        case tree @ g.ClassDef(mods, name, tparams, impl) =>
          //pickleDef
          val sym = tree.symbol
          val tag = TYPEDEF
          registerDef(sym)
          writeByte(tag)
          withLength {
            pickleName(name)
            pickleTree(impl.copy(body = tparams ++ impl.body))
            pickleModifiers(sym)
          }
        case tree @ g.ModuleDef(_, name, impl) =>
          registerDef(tree.symbol)
          writeByte(VALDEF)
            withLength {
              pickleName(name)
              pickleType(impl.tpe)
              writeByte(APPLY)
                withLength {
                  writeByte(SELECT)
                    pickleDottyName("<init>".toTermName)
                    writeByte(NEW)
                      pickleType(impl.tpe.underlying)
                }
              pickleModifiers(tree.symbol)
            }
          writeByte(TYPEDEF)
            withLength {
              pickleDottyName(name.decode.toTypeName.moduleClassName)
              pickleTree(impl)
              pickleModifiers(tree.symbol, moduleClass = true)
            }
        case tree @ g.Template(parents, self, body) =>
          registerDef(tree.symbol)
          writeByte(TEMPLATE)
          val (params, rest0) = tree.body partition {
            case stat: g.TypeDef => stat.symbol.isParameter
            case stat: g.ValOrDefDef =>
              stat.symbol.isParamAccessor && !stat.symbol.isSetter && !stat.symbol.isAccessor
            case _ => false
          }
          val (constructors, rest1) = rest0 partition {
            case stat: g.DefDef => stat.symbol.isConstructor
            case _ => false
          }

          // val primaryCtr = g.treeInfo.firstConstructor(body)
          withLength {
            pickleParams(params)
            parents.foreach(pickleTree)
            if (!self.isEmpty && false) {
              // TODO
            }
            val owner = tree.symbol.owner
            if (owner.isModuleClass) {
              writeByte(SELFDEF)
                pickleName(g.nme.WILDCARD)
                val pre = owner.owner.thisType
                pickleNamedType(pre, owner.module, isType = false)
            }
            pickleStats(constructors ++ rest1)
          }
        case g.This(qual) =>
          if (qual.isEmpty) {
            // Needs to be a ThisType, but can be a TypeRef
            pickleType(g.ThisType(tree.symbol))
          }
          else {
            writeByte(QUALTHIS)
            writeByte(IDENTtpt)
            pickleName(qual)
            val tp = tree.tpe.underlying.widen.typeConstructor
            assert(tp.isInstanceOf[g.TypeRef], s"${tp} ${tp.getClass}")
            pickleType(tp)
          }
        case g.Select(qual, name) =>
          def pickleSelect = {
            writeByte(if (name.isTypeName) SELECTtpt else SELECT)
            pickleNameAndSig(name, tree.tpe)
            pickleTree(qual)
          }
          if (tree.symbol.isConstructor && !tree.symbol.owner.typeParams.isEmpty) {
            val g.TypeRef(_, _, targs) = qual.tpe.widen
            // For some reason we need to pass the type arguments again
            writeByte(TYPEAPPLY)
            withLength {
              pickleSelect
              targs.foreach(pickleType(_))
            }
          }
          else
            pickleSelect
        case g.Apply(fun, args) =>
          fun match {
            case g.TypeApply(fun1, _) if fun1.symbol eq g.definitions.Object_isInstanceOf =>
              // isInstanceOf does not have a parameter list, but scalac sometimes thinks it does
              assert(args.isEmpty)
              pickleTree(fun)
            case _ =>
              writeByte(APPLY)
              withLength {
                pickleTree(fun)
                args.foreach(pickleTree)
              }
          }
        case g.TypeApply(fun, args) =>
          writeByte(TYPEAPPLY)
          withLength {
            pickleTree(fun)
            args.foreach(pickleTpt)
          }
        case g.Ident(name) =>
          // wildcards are pattern bound, need to be preserved as ids.
          if (tree.isTerm && name != g.nme.WILDCARD) {
            // The type of a term Ident should be a TERMREF
            val tp1 = tree.tpe match {
              case tp: g.TypeRef =>
                g.SingleType(tree.symbol.owner.thisType, tree.symbol)
              case tp: g.MethodType => // Happens on calls to LabelDefs
                g.SingleType(tree.symbol.owner.thisType, tree.symbol)
              case tp =>
                tp
            }
            pickleType(tp1)
          } else {
            writeByte(if (tree.isType) IDENTtpt else IDENT)
            pickleName(name)
            pickleType(tree.tpe)
          }
        case tree @ g.DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          val isConstructor = tree.symbol.isConstructor
          def pickleAllParams = {
            if (isConstructor) {
              // <init> tparams are fresh tparams that match the class tparams
              for (tparam <- tree.symbol.owner.typeParams) {
                writeByte(TYPEPARAM)
                withLength {
                  pickleName(tparam.name)
                  pickleType(tparam.info)
                }
              }
            } else {
              pickleParams(tree.tparams)
            }
            for (vparams <- tree.vparamss) {
              writeByte(PARAMS)
              withLength { pickleParams(vparams) }
            }
          }
          val tpt1 =
            if (isConstructor)
              g.TypeTree(g.definitions.UnitTpe)
            else
              tree.tpt
          val rhs1 =
            if (tree.symbol.isPrimaryConstructor)
              g.EmptyTree // TODO: Check if there's no information lost here
            else
              tree.rhs
          pickleDef(DEFDEF, tree.symbol, tpt1, rhs1, pickleAllParams)
        case tree: g.LabelDef =>
          // We run before pattern matching, so the only LabelDefs we see
          // should be translatable to regular defs
          preRegister(tree)
          def pickleLabelParam = {
            writeByte(PARAMS)
            withLength {
              // Empty parameter list
            }
          }
          val tpt = g.TypeTree(tree.tpe)
          writeByte(BLOCK)
          withLength {
            writeByte(APPLY)
            withLength {
              writeByte(TERMREFdirect)
              pickleSymRef(tree.symbol)
            }
            pickleDef(DEFDEF, tree.symbol, tpt, tree.rhs, pickleLabelParam)
          }
        case tree: g.ValDef =>
          pickleDef(VALDEF, tree.symbol, tree.tpt, tree.rhs)
        case g.Import(expr, selectors) =>
          writeByte(IMPORT)
          withLength {
            pickleTree(expr)
            selectors.foreach {
              case g.ImportSelector(name, _, null, _) =>
                writeByte(IMPORTED)
                pickleName(name)
              case g.ImportSelector(name, _, rename, _) =>
                writeByte(IMPORTED)
                pickleName(name)
                writeByte(RENAMED)
                pickleName(rename)
            }
          }
        case tree: g.TypeTree =>
          pickleType(tree.tpe)
        case g.Super(qual, mix) =>
          writeByte(SUPER)
          withLength {
            pickleTree(qual)
            if (!mix.isEmpty) {
              ??? // TODO
            }
          }
        case g.Block(stats, expr) =>
          writeByte(BLOCK)
          stats.foreach(preRegister)
          withLength { pickleTree(expr); stats.foreach(pickleTree) }
        case g.If(cond, thenp, elsep) =>
          writeByte(IF)
          withLength { pickleTree(cond); pickleTree(thenp); pickleTree(elsep) }
        case g.Literal(const1) =>
          pickleConstant {
            tree.tpe match {
              case g.ConstantType(const2) => const2
              case _ => const1
            }
          }
        case g.Match(selector, cases) =>
          writeByte(MATCH)
          withLength { pickleTree(selector); cases.foreach(pickleTree) }
        case g.CaseDef(pat, guard, rhs) =>
          writeByte(CASEDEF)
          withLength { pickleTree(pat); pickleTree(rhs); pickleTreeUnlessEmpty(guard) }
        case g.Bind(name, body) =>
          registerDef(tree.symbol)
          writeByte(BIND)
          withLength {
            pickleName(name); pickleType(tree.symbol.info); pickleTree(body)
          }
        case g.New(tpt) =>
          writeByte(NEW)
          pickleTpt(tpt)
        case g.Throw(expr) =>
          writeByte(APPLY)
            withLength {
              writeByte(TERMREF)
                pickleDottyName("throw".toTermName)
                writeByte(TERMREFpkg)
                  pickleDottyName("<special-ops>".toTermName)
              pickleTree(expr)
            }
        case g.Assign(lhs, rhs) =>
          writeByte(ASSIGN)
          withLength { pickleTree(lhs); pickleTree(rhs) }
        case g.Typed(expr, tpt) =>
          writeByte(TYPED)
          withLength { pickleTree(expr); pickleTpt(tpt) }
        case g.Return(expr) =>
          writeByte(RETURN)
          val fromSymbol = tree.symbol
          withLength { pickleSymRef(fromSymbol); pickleTreeUnlessEmpty(expr) }
        case g.Function(vparams, body) =>
          def pickleFunctionParams = {
            writeByte(PARAMS)
            withLength {
              pickleParams(vparams)
            }
          }
          writeByte(BLOCK)
          preRegister(tree)
          withLength {
            writeByte(LAMBDA)
            withLength {
              pickleNamedType(g.NoPrefix, tree.symbol, isType = false)
              // TODO: tpt for SAMs
            }
            pickleDef(DEFDEF, tree.symbol, g.TypeTree(body.tpe), body, pickleFunctionParams)
          }
        }
      catch {
        case ex: AssertionError =>
          println(s"error when pickling tree $tree")
          throw ex
      }
  }

  def pickleModifiers(sym: g.Symbol, moduleClass: Boolean = false, mutable: Boolean = false,
    field: Boolean = false, methodParam: Boolean = false): Unit = {
    val privateWithin = sym.privateWithin
    if (privateWithin.exists) {
      writeByte(if (sym.isProtected) PROTECTEDqualified else PRIVATEqualified)
      pickleType(privateWithin.toType)
    }
    if (sym.isPrivate || sym.isTypeParameter) writeByte(PRIVATE)
    if (sym.isProtected) if (!privateWithin.exists) writeByte(PROTECTED)
    if ((sym.isFinal) && !(sym.isModule)) writeByte(FINAL)
    if (sym.isCase) writeByte(CASE)
    if (sym.isOverride) writeByte(OVERRIDE)
    // writeByte(INLINE)
    if (sym.isMacro) writeByte(MACRO)
    // if (sym.isJavaStatic) writeByte(STATIC) // TODO ?
    if (sym.isModule) writeByte(OBJECT)
    // if (sym.isLocal) writeByte(LOCAL) // TODO ?
    if (sym.isTypeParameter) writeByte(LOCAL)
    if (sym.isSynthetic) writeByte(SYNTHETIC)
    if (sym.isArtifact) writeByte(ARTIFACT)

    // writeByte(SCALA2X)

    if (sym.isTerm && !moduleClass) {
      if (sym.isImplicit) writeByte(IMPLICIT)
      // if (sym is Erased) writeByte(ERASED)
      if ((sym.isLazy) && !(sym.isModule)) writeByte(LAZY)
      if (sym.hasFlag(scala.reflect.internal.Flags.ABSOVERRIDE)) { writeByte(ABSTRACT); writeByte(OVERRIDE) }
      if (sym.isMutable || sym.isSetter || mutable) writeByte(MUTABLE)
      if (sym.isAccessor && !field) writeByte(FIELDaccessor)
      if (sym.isCaseAccessor) writeByte(CASEaccessor)
      // if (sym.isDefaultParameterized) writeByte(DEFAULTparameterized) // TODO ?
      if (sym.isStable && !methodParam) writeByte(STABLE)
      if ((sym.isParamAccessor) && sym.isSetter) writeByte(PARAMsetter)
    } else {
      if (sym.isSealed) writeByte(SEALED)
      if (sym.isAbstract && !sym.isTypeParameter) writeByte(ABSTRACT)
      if (sym.isTrait) writeByte(TRAIT)
      if (sym.isCovariant) writeByte(COVARIANT)
      if (sym.isContravariant) writeByte(CONTRAVARIANT)
    }
    // TODO:
    // sym.annotations.foreach(pickleAnnotation(sym, _))
  }

  def pickle(trees: List[g.Tree]) = {
    trees.foreach(tree => if (!tree.isEmpty) pickleTree(tree))
    def missing = forwardSymRefs.keysIterator/*.map(_.showLocated)*/.toList
    assert(forwardSymRefs.isEmpty, s"unresolved symbols: $missing%, % when pickling ..."/*${ctx.source}*/)
  }
  def compactify() = {
    buf.compactify()

    def updateMapWithDeltas(mp: mutable.HashMap[g.Symbol, Addr]) =
      for (key <- mp.keysIterator.toBuffer[g.Symbol]) mp(key) = adjusted(mp(key))

    updateMapWithDeltas(symRefs)
  }
}
