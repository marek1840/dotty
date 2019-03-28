package tasty.binary.v2

final class BinaryWriter(buffer: BinaryBuffer = new BinaryBuffer) {
  def bytes: Array[Byte] = {
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
}