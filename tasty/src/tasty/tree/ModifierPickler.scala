package tasty.tree

import tasty.binary.SectionPickler
import tasty.names.PicklerNamePool

abstract class ModifierPickler[Modifier, Name](nameSection: PicklerNamePool[Name],
                                               underlying: SectionPickler)
  extends TreeSectionPickler[Modifier, Name](nameSection, underlying) {

}
