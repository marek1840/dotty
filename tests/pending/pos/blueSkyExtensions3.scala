/** A blue sky sketch how one might evolve extensions to do type classes
 *  without the current gap between functional and OO patterns.
 *  Largely inspired by the way Rust does it.
 *
 *  The main additions (to be worked out in detail) is a self type `This` and
 *  a mechanism that a trait can abstract over companions of classes that implement it.
 *  Companion types and methods are declared using `static` for now, just to
 *  pick some familiar notation.
 *
 *  Ideas about `This` (still vague at the moment)
 *
 *   - Treat it as an additional abstract type in a trait, prefixed by the name of the trait
 *   - An implementing (non-trait) class binds the `This` types of all the traits it implements
 *     to itself.
 *   - Later subclasses do not re-bind `This` types already bound by their superclasses.
 *     (so in that sense, `This`-binding is like trait parameterization, the first implementing
 *     classes determines the self type and the parameters of a trait)
 *   - Paramerized classes have parameterized `This` types (e.g. Functor below).
 */
import Predef.{any2stringadd => _, _}
object blueSkyExtensions {

// Semigroup and Monoid

  trait SemiGroup {
    def + (that: This): This
  }

  trait Monoid extends SemiGroup
  object {
    def unit: This
  }

  extend Int : Monoid {
    def + (that: Int) = this + that
  }
  object {
    static def unit = 0
  }

  class C {
    static Foo
    static Bar
  }
  object Foo extends FooStatic BarStatic BazStatic

  extend String : Monoid {
    def + (that: Int) = this ++ that
  }
  object {
    def unit = ""
  }

// Ord

  trait Ord {
    def compareTo(that: This): Int
    def < (that: This) = compareTo < 0
    def > (that: This) = compareTo > 0
  }

  extend List[type T : Ord] : Ord {
    def compareTo(that: List[T]): Int = (this, that) match {
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs, y :: ys) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs.compareTo(ys)
    }
  }

// Functor and Monad

  trait Functor[A] {
    def map[B](f: A => B): ThisC[B]
  }
  object {
    type ThisC[A] <: Functor[A]
    def pure[A]: ThisC[A]
  }

  // Generically, `pure[A]{.map(f)}^n`
  def develop[A, F[X] : Functor[X]](n: Int, f: A => A): F[A] =
    if (n == 0) Functor.companion[F].pure[A]
    else develop[A, F](n - 1, f).map(f)

  trait Monad[A] extends Functor[A] {
    def flatMap[B](f: A => ThisC[B]): ThisC[B]
    def map[B](f: A => B) = this.flatMap(f.andThen(pure))
  }
  object {
    type ThisC[A] <: Monad[A]
    def pure[A]: ThisC[A]
  }

  extend List[type T] : Monad[T] {
    def flatMap[B](f: A => List[B]): List[B] = this match {
      case x :: xs => f(x) ++ xs.flatMap(f)
      case Nil => Nil
    }
  }
  object {
    type ThisC[A] = List[A]
    def pure[A] = Nil
  }

  extend (type T[X]: Monad[X])[T[type A]] {
    def flatten: T[A] = this.flatMap(identity)
  }

// Iterables

  trait MonoIterable[A] {
    def filter(p: A => Boolean): ThisC[A]
  }
  object {
    type ThisC[A] <: MonoIterable[A]
    def empty: ThisC[A]
    def apply(xs: A*): ThisC[A]
  }

  trait Iterable[A] extends MonoIterable[A] {
    def map[B](f: A => B): ThisC[B]
    def flatMap[B](f: A => ThisC[B]): ThisC[B]
  }
  object {
    type ThisC[A] <: Iterable[A]
  }

  extend String : MonoIterable[Char] {
    def filter(p: Char => Boolean): String = ...
    def map(f: Char => Char): String = ...
  }
  object {
    type ThisC[A] = String
    def empty = ""
    def apply(xs: A*) = xs.mkString
  }

  extend String : Iterable[Char] {
    def map[B](f: Char => B): IndexedSeq[B] = ...
    def flatMap[B](f: Char => IndexedSeq[B]): IndexedSeq[B] = ...
  }
  object {
    type ThisC[A] = IndexedSeq[A]
  }

  extend List[type T] : Iterable[T] {
    def filter(p: T => Boolean): List[T] = ...
    def map[B](f: T => B): List[B] = ...
    def flatMap[B](f: T => List[B]): List[B] = ...
  }
  object {
    type ThisC[A] = List[A]
    def empty = Nil
    def apply(xs: A*) = (xs /: Nil)(_ :: _)
  }

}