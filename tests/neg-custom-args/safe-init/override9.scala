trait Foo {
  def name: String
  val message = "hello, " + name
}

class Bar extends Foo {
  def name = message     // error // error
}