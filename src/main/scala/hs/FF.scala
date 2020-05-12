package hs

trait Foldable[F[_]] {
  def foldl[A, B](as: F[A], z: B, f: (B, A) => B): B
  def foldMap[A, B](as: F[A], f: A => B)(implicit m: Monoid[B]): B =
    foldl(as, m.zero, (b: B, a: A) => m.add(b, f(a)))
}

object FF extends App {
//  def sumOf[F[_]](ns: F[Int])(implicit ff: Foldable[F]) =
//    ff.foldl(ns, 0, (x: Int, y: Int) => x + y)

  implicit val listFoldable: Foldable[List] = new Foldable[List] {
    override def foldl[A, B](as: List[A], z: B, f: (B, A) => B): B =
      as.foldLeft(z)(f)
  }

  implicit val maybeFoldable: Foldable[Option] = new Foldable[Option] {
    override def foldl[A, B](as: scala.Option[A], z: B, f: (B, A) => B): B =
      as.foldLeft(z)(f)
  }

  implicit val someFoldable: Foldable[Some] = new Foldable[scala.Some] {
    override def foldl[A, B](as: scala.Some[A], z: B, f: (B, A) => B): B =
      as.foldLeft(z)(f)
  }

  implicit val setFoldable: Foldable[Set] = new Foldable[Set] {
    override def foldl[A, B](as: Set[A], z: B, f: (B, A) => B): B =
      as.foldLeft(z)(f)
  }

//  val sumOfOneTwo = sumOf(List(1, 2, 3))
//  println(sumOfOneTwo)

  implicit val sumMonoid: Monoid[Sum] = new Monoid[Sum] {
    def zero = Sum(0)
    def add(a: Sum, b: Sum) = Sum(a.value + b.value)
  }

  implicit val productMonoid = new Monoid[Product] {
    def zero = Product(1)
    def add(a: Product, b: Product) = Product(a.value * b.value)
  }

  def mapReduce[F[_], A, B](as: F[A], f: A => B)(implicit ff: Foldable[F],
                                                 m: Monoid[B]) =
    ff.foldMap(as, f)

  val sumOf123 = mapReduce(List(1, 2, 3), Sum)
  val product123 = mapReduce(List(4, 5, 6), Product)
  val product1234 = mapReduce(Set(4, 5, 6), Sum)
//  val t = mapReduce(None, Sum)
//  val t1 = mapReduce(Some(5), Sum)
  println(sumOf123.value)
  println(product123)
  println(product1234)
//  println(t)
//  println(t1)
}

trait Monoid[M] {
  def zero: M
  def add(m1: M, m2: M): M
}
trait Functor[F[_]] {
  def map[A, B](a: F[A])(f: A => B): F[B]
}

case class Sum(value: Int)
case class Product(value: Int)

trait Apart1[F] {
  type T
  type W[X]

  def apply(f: F): W[T]
}

object Apart1 {

  def apply[F](implicit apart1: Apart1[F]) = apart1

  type Aux1[FA, A, F[_]] = Apart1[FA] {
    type T = A
    type W[X] = F[X]
  }

  implicit def mk[F[_], A]: Aux1[F[A], A, F] = new Apart1[F[A]] {
    type T = A
    type W[X] = F[X]

    override def apply(f: F[A]): W[T] = f
  }

  def mapZero[Thing, F[_], A](thing: Thing)(f: Functor[F], m: Monoid[A])(
    implicit apart: Apart1.Aux1[Thing, A, F]
  ): F[A] =
    f.map(apart(thing))(_ => m.zero)

//  case class Foo1[V](value: V)
//
//  def zero1[T, A](t: T)(m: Monoid[A])(
//    implicit inner: Inner.Aux[T, A]
//  ): inner.T = m.zero

  def main(args: Array[String]): Unit = {
    val functor = new Functor[List] {
      override def map[A, B](a: List[A])(f: A => B): List[B] = a map f
    }
    println(mapZero(List(List(1)))(functor, new Monoid[List[Int]] {
      override def zero: List[Int] = List.empty
      override def add(m1: List[Int], m2: List[Int]): List[Int] = m1 ++ m2
    }))

    println(mapZero(List(1))(functor, new Monoid[Int] {
      override def zero: Int = 0
      override def add(m1: Int, m2: Int): Int = m1 + m2
    }))

  }
}
