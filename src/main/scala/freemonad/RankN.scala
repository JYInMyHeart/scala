package freemonad

object RankN extends App {
  def applyToTuple[A](f: List[A] => Int, t: (List[A], List[A])) =
    (f(t._1), f(t._2))

  def f[A](a: List[A]): Int = a.length

  type Id[A] = A

  trait ~>[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }
  val singletonList = new (Id ~> List) {
    def apply[A](a: A): List[A] = List(a)
  }

  def apply[A, B](f: Id ~> List, b: B, s: String): (List[B], List[String]) =
    (f(b), f(s))

  trait -->[G[_], F[_]] {
    def apply[T](fa: G[T]): F[T]
  }

  val s = new (Id --> Id) {
    override def apply[T](fa: T) = fa
  }

  implicit val toMaybe = new (Id --> Option) {
    override def apply[T](fa: T) = Some(fa)
  }

  def r[T](b: String, c: Int)(implicit f: Id --> Option) = {
    if (f(true).get) {
      f(b)
    } else {
      f(c)
    }
  }
  println(r("1", 2))

  def r[T](f: Id --> Id,
           b: String,
           c: Int,
           d: Boolean): Id[_ >: String with Int with Boolean] = {
    if (f(d)) {
      f(b)
    } else {
      if (f(d))
        f(c)
      else
        f(d)
    }
  }

  println(applyToTuple(f, (List(1, 2, 3), List("1", "2"))))
  println(apply(singletonList, 2, "2"))
  println(r(s, "1", 2, d = false))

}
trait Univ1[Bound1, Body[_]] {
  def Apply[T1 <: Bound1]: Body[T1]
}

trait Univ2[Bound1, Bound2, Body[_, _]] {
  def Apply[T1 <: Bound1, T2 <: Bound2]: Body[T1, T2]
}

object Test extends App {
  def id[T](x: T) = x

  type Id[T] = T => T
  val id = new Univ1[Any, Id] { def Apply[T <: Any]: Id[T] = id[T] _ }

  val idString = id.Apply[String]
  val idStringList = id.Apply[List[String]]

  println(idString("Foo"))
  println(idStringList(List("Foo", "Bar", "Baz")))

  type Double[T] = T => (T, T)
  val double = new Univ1[Any, Double] {
    def Apply[T <: Any]: Double[T] = (x: T) => (x, x)
  }

  val doubleString = double.Apply[String]
  val doubleStringList = double.Apply[List[String]]

  println(doubleString("Foo"))
  println(doubleStringList(List("Foo", "Bar", "Baz")))
}
