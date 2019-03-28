package tasty.binary.v2

import dotty.tools.dotc.util.Util.dble

final class BinaryBuffer(initialSize: Int = 32) {
  private var buffer: Array[Byte] = new Array[Byte](initialSize)
  private[v2] var offset = 0

  def bytes: Array[Byte] = buffer.take(offset)

  def writeByte(value: Int): Unit = {
    ensureCapacity(1)
    buffer(offset) = value.toByte
    offset += 1
  }

  def writeBytes(bytes: Array[Byte], amount: Int): Unit = {
    ensureCapacity(amount)
    System.arraycopy(bytes, 0, bytes, offset, amount)
    offset += amount
  }

  def ensureCapacity(n: Int): Unit = {
    while (offset + n >= buffer.length) buffer = dble(buffer)
  }
}


