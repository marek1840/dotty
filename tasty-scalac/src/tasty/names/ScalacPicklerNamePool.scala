package tasty.names

import tasty.binary.SectionPickler

import scala.io.Codec
import scala.tools.nsc.Global


final class ScalacPicklerNamePool(output: SectionPickler)(implicit g: Global) extends PicklerNamePool[TastyName](output) {

  protected def pickleSimpleName(name: String): Unit = {
    val bytes = Codec.toUTF8(name.toCharArray, 0, name.length)
    pickleUtf8(bytes)
  }

  override protected def pickle(name: TastyName): Unit = name match {
    case TastyName.UTF8(name) =>
      pickleSimpleName(name)
    case TastyName.Qualified(prefix, suffix) =>
      pickleQualifiedName(pickleName(prefix), pickleName(suffix))
    case TastyName.Signed(name, parameterNames, returnName) =>
      val paramNames = parameterNames.map(param => pickleName(param))
      pickleSignedName(pickleName(name), pickleName(returnName), paramNames)
    case _ =>
  }

}