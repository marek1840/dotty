package tasty.binary

import dotty.tools.dotc.core.tasty.TastyHash
import dotty.tools.dotc.util.Util.dble

class BinaryPickler(initialSize: Int = 64) {
  protected var buffer = new Array[Byte](initialSize)
  protected var length = 0

  final def size: Int = length

  final def bytes: Array[Byte] = buffer.take(length)

  final def pickle(subsection: BinaryPickler): Unit = {
    pickleNat(subsection.size)
    pickleBytes(subsection.bytes, subsection.size)
  }


  final def pickleByte(b: Int): Unit = {
    ensureCapacity(1)
    buffer(length) = b.toByte
    length += 1
  }

  final def pickleBytes(bytes: Array[Byte]): Unit = pickleBytes(bytes, bytes.length)

  final def pickleBytes(bytes: Array[Byte], amount: Int): Unit = {
    ensureCapacity(amount)
    System.arraycopy(bytes, 0, buffer, length, amount)
    length += amount
  }

  final def pickleNat(value: Int): Unit = pickleNat(value.toLong & 0x00000000FFFFFFFFL)

  final def pickleNat(value: Long): Unit = {
    def picklePrefix(x: Long): Unit = {
      val y = x >>> 7
      if (y != 0L) picklePrefix(y)
      pickleByte((x & 0x7f).toInt)
    }

    val y = value >>> 7
    if (y != 0L) picklePrefix(y)
    pickleByte(((value & 0x7f) | 0x80).toInt)
  }

  final def pickleInteger(value: Int): Unit = pickleLong(value.toLong)

  final def pickleLong(value: Long): Unit = {
    def picklePrefix(x: Long): Unit = {
      val y = x >> 7
      if (y != 0L - ((x >> 6) & 1)) picklePrefix(y)
      pickleByte((x & 0x7f).toInt)
    }

    val y = value >> 7
    if (y != 0L - ((value >> 6) & 1)) picklePrefix(y)
    pickleByte(((value & 0x7f) | 0x80).toInt)
  }

  final def pickleUncompressedLong(value: Long): Unit = {
    var y = value
    val bytes = new Array[Byte](8)
    for (i <- 7 to 0 by -1) {
      bytes(i) = (y & 0xff).toByte
      y = y >>> 8
    }
    pickleBytes(bytes)
  }

  private def ensureCapacity(n: Int): Unit = {
    while (length + n >= buffer.length) buffer = dble(buffer)
  }
}

object BinaryPickler {
  def hashOf(pickler: BinaryPickler): Long = TastyHash.pjwHash64(pickler.bytes)
}