class A {
  val f: Int = 10

  class B {
    val a = f
  }

  println(new B)
}

class C extends A {
  override val f: Int = 20   // error
}