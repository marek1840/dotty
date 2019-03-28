package tasty.binary.v2


final class BinaryWriter(buffer: BinaryBuffer = new BinaryBuffer) {
  private val compressor = new Compressor(buffer)

  def bytes: Array[Byte] = {
    compressor.compress()

    buffer.bytes
  }

  def writeNat(value: Int): Unit = writeLongNat(value.toLong)

  def writeLongNat(value: Long): Unit = {
    def writePrefix(x: Long): Unit = {
      val y = x >>> 7
      if (y != 0L) writePrefix(y)
      buffer.writeByte((x & 0x7f).toInt)
    }

    val y = value >>> 7
    if (y != 0L) writePrefix(y)
    buffer.writeByte(((value & 0x7f) | 0x80).toInt)
  }

  def writeInteger(value: Int): Unit = writeLong(value.toLong)

  def writeLong(value: Long): Unit = {
    def writePrefix(x: Long): Unit = {
      val y = x >> 7
      if (y != 0L - ((x >> 6) & 1)) writePrefix(y)
      buffer.writeByte((x & 0x7f).toInt)
    }

    val y = value >> 7
    if (y != 0L - ((value >> 6) & 1)) writePrefix(y)
    buffer.writeByte(((value & 0x7f) | 0x80).toInt)
  }

  def write(op: => Unit): Unit = {
    // reserve bytes for writing length
    val lengthOffset = buffer.offset
    val sectionStart = buffer.offset + BinaryWriter.lengthWidth

    // write subsection
    buffer.offset = sectionStart
    op
    val sectionEnd = buffer.offset

    // write subsection length
    val sectionLength = sectionEnd - sectionStart
    buffer.offset = lengthOffset
    writeNat(sectionLength)

    val paddedBytes = sectionStart - buffer.offset
    if (paddedBytes > 0) {
      compressor.addPadding(buffer.offset, paddedBytes)
    }

    buffer.offset = sectionEnd
  }
}

object BinaryWriter {
  private[BinaryWriter] val lengthWidth = 4
}