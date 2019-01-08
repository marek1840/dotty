import scala.quoted._

import scala.quoted.Toolbox

object Test {

  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make
    val ack3 = tb.run(ackermann(3))
    println(ack3(1))
    println(ack3(2))
    println(ack3(3))
    println(ack3(4))
  }

  def ackermann(m: Int): Staged[Int => Int] = {
    if (m == 0) '{ n => n + 1 }
    else '{ n =>
      def `ackermann(m-1)`(n: Int): Int = ~{ val ack = ackermann(m - 1); ack('(n)) } // Expr[Int => Int] applied to Expr[Int]
      def `ackermann(m)`(n: Int): Int =
        if (n == 0) `ackermann(m-1)`(1) else `ackermann(m-1)`(`ackermann(m)`(n - 1))
      `ackermann(m)`(n)
    }
  }

}
