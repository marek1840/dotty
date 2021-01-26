package tasty.names

import tasty.binary.BinaryInput

import scala.collection.mutable

abstract class NameSectionUnpickler[Name]  {

  protected def createUnpickler(names: Seq[Name]): NameUnpickler[Name]

  final def unpickle(input: BinaryInput): Seq[Name] = {
    val names = mutable.ArrayBuffer[Name]()
    val unpickler = createUnpickler(names)

    input.untilEndReached {
      names += unpickler.unpickle(_)
    }

    names
  }
}