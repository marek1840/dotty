
import scala.quoted._

object Test {

  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make

    def test[T](clazz: java.lang.Class[T]): Unit = {
      def lclazz: Staged[Class[T]] = clazz.toExpr
      def name: Staged[String] = '{ (~lclazz).getCanonicalName }
      println()
      println(tb.show(name))
      println(tb.run(name))
    }

    // primitive arrays
    test(classOf[Array[Int]])
    test(classOf[Array[Long]])
    test(classOf[Array[Float]])
    test(classOf[Array[Double]])
  }

}
