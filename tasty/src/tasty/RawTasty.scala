package tasty

import java.util.UUID

import dotty.tools.dotc.core.tasty.TastyFormat.{MajorVersion, MinorVersion, header}
import tasty.binary.BinaryInput
import tasty.names.NameRef

final class RawTasty(val id: UUID, val nameSection: BinaryInput, val sections: Map[NameRef, BinaryInput])

object RawTasty {

  def apply(input: BinaryInput): RawTasty = {
    val id = readHeader(input)
    val nameSection = input.subsection()
    val sections = readSections(input)
    new RawTasty(id, nameSection, sections)
  }

  private def readHeader(input: BinaryInput): UUID = {
    import input._
    if (header.exists(_ != readByte())) throw new IllegalStateException("Not a tasty file")

    val major = readNat()
    val minor = readNat()
    if (major != MajorVersion && minor > MinorVersion)
      throw new IllegalStateException(
        s"""TASTy signature has wrong version.
           | expected: $MajorVersion.$MinorVersion
           | found   : $major.$minor""".stripMargin)

    new UUID(readUncompressedLong(), readUncompressedLong())
  }

  private def readSections(input: BinaryInput): Map[NameRef, BinaryInput] = {
    def section(input: BinaryInput): (NameRef, BinaryInput) = {
      val nameRef = new NameRef(input.readNat())
      val subsection = input.subsection()
      nameRef -> subsection
    }

    val sections = input.readSequence(section)

    sections.toMap
  }
}