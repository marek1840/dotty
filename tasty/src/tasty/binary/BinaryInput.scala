package tasty.binary

import scala.collection.mutable

final class BinaryInput(bytes: Array[Byte], start: Int, end: Int) {
  def this(bytes: Array[Byte]) = this(bytes, 0, bytes.length)

  def ifNotEmpty[A](fn: BinaryInput => A): Option[A] =
    if (offset == end) None
    else Some(fn(this))

  def readSequence[A](fn: BinaryInput => A): Seq[A] = {
    val buffer = mutable.ArrayBuffer[A]()

    while (offset < end) buffer += fn(this)
    if (offset > end) throw new IllegalStateException()

    buffer
  }

  private var offset = start

  def subsection(): BinaryInput = {
    val length = readNat()
    offset += length
    new BinaryInput(bytes, offset - length, offset)
  }

  def untilEndReached(fn: BinaryInput => Unit): Unit = {
    while (offset < end) fn(this)
    if (offset > end) throw new IllegalStateException()
  }

  def nextByte: Int = bytes(offset) & 0xff

  def skip(length: Int): Unit = offset += length

  def readByte(): Int = {
    val result = nextByte
    offset += 1
    result
  }

  def readBytes(): Array[Byte] = readBytes(end - offset)

  def readBytes(n: Int): Array[Byte] = {
    val result = new Array[Byte](n)
    System.arraycopy(bytes, offset, result, 0, n)
    offset += n
    result
  }

  def readNat(): Int = readLongNat().toInt

  def readInt(): Int = readLongInt().toInt

  def readLongNat(): Long = {
    var b = 0L
    var x = 0L
    do {
      b = bytes(offset)
      x = (x << 7) | (b & 0x7f)
      offset += 1
    } while ((b & 0x80) == 0)
    x
  }

  def readLongInt(): Long = {
    var b = bytes(offset)
    var x: Long = (b << 1).toByte >> 1 // sign extend with bit 6.
    offset += 1
    while ((b & 0x80) == 0) {
      b = bytes(offset)
      x = (x << 7) | (b & 0x7f)
      offset += 1
    }
    x
  }

  def readUncompressedLong(): Long = {
    var x: Long = 0
    for (i <- 0 to 7)
      x = (x << 8) | (readByte() & 0xff)
    x
  }
}
