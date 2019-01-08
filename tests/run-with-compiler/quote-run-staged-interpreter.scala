import scala.quoted._

enum Exp {
  case Num(n: Int)
  case Plus(e1: Exp, e2: Exp)
  case Var(x: String)
  case Let(x: String, e: Exp, in: Exp)
}

object Test {
  import Exp._

  def compile(e: Exp, env: Map[String, Expr[Int]], keepLets: Boolean): Staged[Int] = {
    def compileImpl(e: Exp, env: Map[String, Expr[Int]]): Expr[Int] = e match {
      case Num(n) => n.toExpr
      case Plus(e1, e2) => '(~compileImpl(e1, env) + ~compileImpl(e2, env))
      case Var(x) => env(x)
      case Let(x, e, body) =>
        if (keepLets)
          '{ val y = ~compileImpl(e, env); ~compileImpl(body, env + (x -> '(y))) }
        else
          compileImpl(body, env + (x -> compileImpl(e, env)))
    }
    compileImpl(e, env)
  }


  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make
    import tb._

    val exp = Plus(Plus(Num(2), Var("x")), Num(4))
    val letExp = Let("x", Num(3), exp)

    val res1: Staged[Int => Int] = '{ (x: Int) => ~compile(exp, Map("x" -> '(x)), false) }

    println(show(res1))

    val fn = run(res1)
    println(fn(0))
    println(fn(2))
    println(fn(3))

    println("---")

    def res2: Staged[Int] = compile(letExp, Map(), false)
    println(show(res2))
    println(run(res2))

    println("---")

    val res3: Staged[Int] = compile(letExp, Map(), true)
    println(show(res3))
    println(run(res3))
  }
}
