package tasty.tree

import tasty.binary.SectionPickler
import tasty.names.{TastyName, ScalacPicklerNamePool}

import scala.tools.nsc.Global

final class ScalacModifierPickler(nameSection: ScalacPicklerNamePool,
                                  underlying: SectionPickler)
                                 (implicit val g: Global)
  extends ModifierPickler[Global#Symbol, TastyName](nameSection, underlying) {

  override def pickle(value: Global#Symbol): Unit = {}
}
