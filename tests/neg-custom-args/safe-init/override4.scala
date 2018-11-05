import scala.annotation.cold
import scala.annotation.init
import scala.collection.mutable

class Foo {
  private val map: mutable.Map[Int, String] = mutable.Map.empty

  @init
  def enter(k: Int, v: String) = map(k) = v
}

class Bar extends Foo {
  enter(1, "one")
  enter(2, "two")
}

class Bar2 extends Bar {
  val mymap: mutable.Map[Int, String] = mutable.Map.empty

  override def enter(k: Int, v: String) = {   // error
    mymap(k) = v                   // error
  }
}

class Foo1 {
  val map: mutable.Map[Int, String] = mutable.Map.empty

  @cold
  def enter(k: Int, v: String) = map(k) = v // error // error
}


abstract class Foo2 {
  def map: mutable.Map[Int, String]

  @cold
  def enter(k: Int, v: String) = map(k) = v  // error // error
}

class Qux extends Foo2 {
  val map: mutable.Map[Int, String] = mutable.Map.empty
}