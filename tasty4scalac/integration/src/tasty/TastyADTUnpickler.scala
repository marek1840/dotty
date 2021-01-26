package tasty

import tasty.binary.BinaryInput
import tasty.names.{TastyName, TastyNameSectionUnpickler}

object TastyADTUnpickler extends TastyUnpickler[TastyName, TastyADT] {
  override protected def unpickleNames(input: BinaryInput): Seq[TastyName] = TastyNameSectionUnpickler.unpickle(input)

  override protected def unpickle(names: Seq[TastyName], sections: Map[TastyName, BinaryInput]): TastyADT =
    new TastyADT(names)
}
