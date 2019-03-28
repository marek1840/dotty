package tasty.binary.v2

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class BinaryWriterTest {
  @Test
  def writeCompressedIntegers(): Unit = {
    for (byteCount <- 1 to 4) {
      val writer = new BinaryWriter()
      val value = 1 << ((byteCount - 1) * 8)

      writer.writeInteger(value)

      val written = writer.bytes
      assertEquals(byteCount, written.length)
    }
  }

  @Test
  def writeCompressedNaturalNumbers(): Unit = {
    for (byteCount <- 1L to 4) {
      val writer = new BinaryWriter()
      val value = 1L << ((byteCount - 1) * 8)

      writer.writeLongNat(value)

      val written = writer.bytes
      assertEquals(byteCount, written.length)
    }
  }


}
