package tasty.tree.types

import tasty.binary.SectionPickler
import tasty.names.{TastyName, ScalacNameConversions, ScalacPicklerNamePool}

import scala.tools.nsc.Global

final class ScalacConstantPickler(val nameSection: ScalacPicklerNamePool,
                                  output: SectionPickler)
                                (implicit g: Global)
  extends ConstantPickler[Global#Constant, TastyName](nameSection, output) with ScalacNameConversions{

  override def pickle(constant: Global#Constant): Unit = constant.tag match {
    // TODO   case g.NoTag => ???
    case g.UnitTag => pickleUnitConst()
    case g.BooleanTag => pickleBooleanConst(constant.booleanValue)
    case g.ByteTag => pickleByteConst(constant.byteValue)
    case g.ShortTag => pickleShortConst(constant.shortValue)
    case g.IntTag => pickleIntConst(constant.intValue)
    case g.LongTag => pickleLongConst(constant.longValue)
    case g.FloatTag => pickleFloatConst(constant.floatValue)
    case g.DoubleTag => pickleDoubleConst(constant.doubleValue)
    case g.CharTag => pickleCharConst(constant.charValue)
    case g.StringTag => pickleStringConst(constant.stringValue)
    case g.NullTag => pickleNullConst()
    case g.ClazzTag => pickleClassConst()
    case g.EnumTag => pickleEnumConst()
    case _ => throw new UnsupportedOperationException(s"Cannot pickle constant [$constant]")
  }
}
