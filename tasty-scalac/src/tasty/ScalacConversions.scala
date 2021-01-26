package tasty

import scala.tools.nsc.Global

trait ScalacConversions {
  val g: Global

  protected implicit def convertType(tp: Global#Type): g.Type = tp.asInstanceOf[g.Type]

  protected implicit def convertTree(tp: Global#Tree): g.Tree = tp.asInstanceOf[g.Tree]

  protected implicit def convertSymbol(tp: Global#Symbol): g.Symbol = tp.asInstanceOf[g.Symbol]
}
