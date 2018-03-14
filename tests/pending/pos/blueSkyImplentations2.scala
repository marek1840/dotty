object runtime {

  trait TypeClass {
    type This
    type StaticPart[This]
  }

  trait Implementation[From] {
    type This = From
    type Implemented <: TypeClass
    def inject(x: From): Implemented { type This = From }
  }

  class CompanionOf[T] { type StaticPart[_] }

  def instance[From, To <: TypeClass](
      implicit ev1: Implementation[From] { type Implemented = To },
      ev2: CompanionOf[To]): Implementation[From] { type Implemented = To } & ev2.StaticPart[From] =
    ev1.asInstanceOf

  implicit def inject[From](x: From)(
      implicit ev1: Implementation[From]): ev1.Implemented { type This = From } =
    ev1.inject(x)
}

object semiGroups {
  import runtime._

  trait SemiGroup extends TypeClass {
    def add (that: This): This
  }

  trait Monoid extends SemiGroup {
    type StaticPart[This] <: MonoidStatic[This]
  }
  abstract class MonoidStatic[This] { def unit: This }

  implicit def companionOfMonoid: CompanionOf[Monoid] {
    type StaticPart[X] = MonoidStatic[X]
  } = new CompanionOf[Monoid] {
    type StaticPart[X] = MonoidStatic[X]
  }

  implicit object extend_Int_Monoid extends MonoidStatic[Int] with Implementation[Int] {
    type Implemented = Monoid
    def unit: Int = 0
    def inject($this: Int) = new Monoid {
      type This = Int
      def add (that: This): This = $this + that
    }
  }

  implicit object extend_String_Monoid extends MonoidStatic[String] with Implementation[String] {
    type Implemented = Monoid
    def unit = ""
    def inject($this: String): Monoid { type This = String } =
      new Monoid {
        type This = String
        def add (that: This): This = $this ++ that
      }
  }

  def sum[T](xs: List[T])(implicit $ev: Implementation[T] { type Implemented = Monoid } ) = {
    (instance[T, Monoid].unit /: xs)((x, y) => (x) add y)
  }
}