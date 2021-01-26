package tasty.tree.types

import dotty.tools.dotc.core.tasty.TastyFormat._
import tasty.binary.SectionPickler
import tasty.names.PicklerNamePool
import tasty.tree.TreeSectionPickler

abstract class ConstantPickler[Constant, Name](nameSection: PicklerNamePool[Name],
                                               underlying: SectionPickler)
  extends TreeSectionPickler[Constant, Name](nameSection, underlying) {

  def pickle(constant: Constant): Unit

  protected final def pickleUnitConst(): Unit = tagged(UNITconst) {}

  protected final def pickleNullConst(): Unit = tagged(NULLconst) {}

  protected final def pickleBooleanConst(value: Boolean): Unit = tagged(if (value) TRUEconst else FALSEconst) {}

  protected final def pickleByteConst(value: Byte): Unit = tagged(BYTEconst) {
    pickleInteger(value)
  }

  protected final def pickleShortConst(value: Short): Unit = tagged(SHORTconst) {
    pickleInteger(value)
  }

  protected final def pickleIntConst(value: Int): Unit = tagged(INTconst) {
    pickleInteger(value)
  }

  protected final def pickleLongConst(value: Long): Unit = tagged(LONGconst) {
    pickleLong(value)
  }

  protected final def pickleFloatConst(value: Float): Unit = tagged(FLOATconst) {
    pickleInteger(java.lang.Float.floatToRawIntBits(value))
  }

  protected final def pickleDoubleConst(value: Double): Unit = tagged(DOUBLEconst) {
    pickleLong(java.lang.Double.doubleToRawLongBits(value))
  }

  protected final def pickleCharConst(value: Char): Unit = tagged(CHARconst) {
    pickleNat(value)
  }

  protected final def pickleStringConst(value: Name): Unit = tagged(STRINGconst) {
    pickleName(value)
  }

  protected final def pickleClassConst(): Unit = ???

  protected final def pickleEnumConst(): Unit = ???

  protected final def pickleSymbolConst(): Unit = ???
}
