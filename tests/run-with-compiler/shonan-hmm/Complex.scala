
import scala.quoted._

case class Complex[T](re: T, im: T)

object Complex {
  implicit def complexIsLiftable[T: Type: Liftable]: Liftable[Complex[T]] = new Liftable {
    def toExpr(c: Complex[T])(implicit st: StagingContext): Expr[Complex[T]] = '{ Complex(~c.re.toExpr, ~c.im.toExpr) }
  }

 def of_complex_expr(x: Expr[Complex[Int]])(implicit st: StagingContext): Complex[Expr[Int]] = Complex('((~x).re), '((~x).im))
 def of_expr_complex(x: Complex[Expr[Int]]): Staged[Complex[Int]] = '(Complex(~x.re, ~x.im))

}
