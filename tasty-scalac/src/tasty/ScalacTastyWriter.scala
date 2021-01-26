package tasty

import dotty.tools.dotc.core.tasty.TastyFormat._
import tasty.binary.BinaryPickler.hashOf
import tasty.binary.{BinaryPickler, SectionPickler}
import tasty.names.{ScalacNameConversions, ScalacPicklerNamePool, TastyName}
import tasty.tree.terms.ScalacTreePickler

import scala.tools.nsc.Global

final class ScalacTastyWriter(implicit val g: Global) extends TastyWriter with ScalacNameConversions{
  override type Name = TastyName
  private val namesSection = new SectionPickler
  private val namePool = new ScalacPicklerNamePool(namesSection)

  private val treeSection = new SectionPickler
  private val treeSectionNameRef = namePool.pickleName("ASTs")
  private val treePickler = new ScalacTreePickler(namePool, treeSection)

  def write(tree: Global#Tree): Unit = treePickler.pickle(tree)

  def output(): Array[Byte] = {
    val totalSize = TastyWriter.headerBytes + namesSection.size + treeSection.size
    val output = new BinaryPickler(totalSize) // TODO can be streamed instead of copying arrays in memory

    writeHeader(output)
    output.pickle(namesSection)
    output.pickleNat(treeSectionNameRef.index)
    output.pickle(treeSection)

    output.bytes
  }

  private def writeHeader(output: BinaryPickler): Unit = {
    val namesHash = hashOf(namesSection)
    val treeHash = hashOf(treeSection)
    val uuidLow: Long = namesHash ^ treeHash
    val uuidHi = (Map() - treeSectionNameRef).values.foldLeft(0L)(_ ^ hashOf(_))

    for (ch <- header) output.pickleByte(ch.toByte)
    output.pickleNat(MajorVersion)
    output.pickleNat(MinorVersion)
    output.pickleUncompressedLong(uuidLow)
    output.pickleUncompressedLong(uuidHi)
  }

}