class Foo(n: Cold[String]) {
  foo(new Foo("Jack"))         // recursive creation

  val name: String = n
  name.length                 // error

  private def foo(o: Foo) = o.name
}