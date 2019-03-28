package tasty.binary.v2

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class BinaryWriterTest {
  @Test
  def writesCompressedSections(): Unit = {
    val writer = new BinaryWriter()
    val sectionCount = 5
    val expectedSectionSize = 2

    for (n <- 1 to sectionCount) {
      writer.write {
        writer.writeInteger(n)
      }
    }

    assertEquals(sectionCount * expectedSectionSize, writer.bytes.length)
  }

  @Test
  def consecutiveCompressionsAreIdempotent(): Unit = {
    val writer = new BinaryWriter()

    writer.write {
      writer.writeInteger(1)
    }

    assertArrayEquals(writer.bytes, writer.bytes)
  }

  @Test
  def writeCompressedIntegers(): Unit = {
    for (byteCount <- 1 to 4) {
      val writer = new BinaryWriter()
      val value = 1 << ((byteCount - 1) * 8)

      writer.writeInteger(value)

      assertEquals(byteCount, writer.bytes.length)
    }
  }

  @Test
  def writeCompressedNaturalNumbers(): Unit = {
    for (byteCount <- 1L to 4) {
      val writer = new BinaryWriter()
      val value = 1L << ((byteCount - 1) * 8)

      writer.writeLongNat(value)

      assertEquals(byteCount, writer.bytes.length)
    }
  }
}
