package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames.nme

import scala.tasty.trees
import scala.tasty.trees.Import.ImportSelector

object Import {

  def apply(tree: tpd.Tree)(implicit ctx: Context): trees.Import = Impl(tree, ctx)

  def unapplyImport(tree: trees.Tree): Option[trees.Import.Data] = tree match {
    case Impl(Trees.Import(expr, selectors), ctx) => Some(Term(expr)(ctx), selectors.map(importSelector(_)(ctx)))
    case _ => None
  }

  private def importSelector(tree: untpd.Tree)(implicit ctx: Context): ImportSelector = tree match {
    case id@Trees.Ident(_) => ImportSelector.Simple(Id(id))
    case Trees.Thicket((id@Trees.Ident(_)) :: Trees.Ident(nme.WILDCARD) :: Nil) => ImportSelector.Omit(Id(id))
    case Trees.Thicket((id1@Trees.Ident(_)) :: (id2@Trees.Ident(_)) :: Nil) => ImportSelector.Rename(Id(id1), Id(id2))
  }

  private case class Impl(tree: tpd.Tree, ctx: Context) extends trees.Import with Positioned {
    override def toString: String = {
      import Toolbox.extractor
      val trees.Import(pkg, body) = this
      s"Import($pkg, $body)"
    }
  }

}