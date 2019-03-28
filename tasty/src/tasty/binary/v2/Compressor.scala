package tasty.binary.v2

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class Compressor(buffer: BinaryBuffer) {
  private val paddings: mutable.Buffer[Padding] = ListBuffer()

  def addPadding(address: Int, size: Int): Unit = paddings += Padding(address, size)

  def compress(): Unit = {
    @tailrec
    def compress(ps: Seq[Padding]): Unit = ps match {
      case Seq() =>
      case (p: Padding) +: tail => removePadding(p, tail)
      case tail => compress(tail)
    }

    compress(paddings)
    paddings.clear()
  }

  private def removePadding(padding: Padding, following: Seq[Padding]): Unit = {
    var totalPadding = padding.size
    var at = padding.address + totalPadding

    @tailrec
    def step(remaining: Seq[Padding]): Unit = {
      remaining match {
        case Seq() =>
        case Padding(address, size) +: tail =>
          while (at < address) {
            buffer.move(at, at - totalPadding)
            at += 1
          }
          at += size
          totalPadding += size
          step(tail)
      }
    }

    step(following)
    buffer.offset -= totalPadding
  }


  private case class Padding(address: Int, size: Int)

}