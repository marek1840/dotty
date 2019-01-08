import scala.quoted._

object Test {

  implicit def dummy: StagingContext = ???

  def f[T](x: Expr[T])(implicit t: Type[T]) = '{
    val y: t.unary_~ = x.unary_~
    val z = ~x
  }

  f('(2))('[Int])
  f('{ true })('[Boolean])

  def g(es: Expr[String], t: Type[String]) =
    f('{ (~es + "!") :: Nil })('[List[~t]])
}

