import scala.quoted._

object Macros {

  inline def assert(expr: => Boolean): Unit =
    ~assertImpl('(expr))

  def assertImpl(expr: Expr[Boolean]): Staged[Unit] =
    '{ if !(~expr) then throw new AssertionError(s"failed assertion: ${~showExpr(expr)}") }

  def showExpr[T](expr: Expr[T]): Staged[String] = expr.toString.toExpr

  inline def power(inline n: Int, x: Double) = ~powerCode(n, '(x))

  def powerCode(n: Int, x: Expr[Double]): Staged[Double] =
    if (n == 0) '(1.0)
    else if (n == 1) x
    else if (n % 2 == 0) '{ { val y = ~x * ~x; ~powerCode(n / 2, '(y)) } }
    else '{ ~x * ~powerCode(n - 1, x) }
}

class Test {

  val tb = Toolbox.make
  tb.run {
    val program = '{
      import Macros._

      val x = 1
      assert(x != 0)

      ~assertImpl('(x != 0))

      val y = math.sqrt(2.0)

      power(3, y)

      ~powerCode(3, '{math.sqrt(2.0)})
    }

    program
  }
}
