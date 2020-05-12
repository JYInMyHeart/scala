package catsLearn

import scala.collection.{GenTraversableLike, TraversableLike}

object Test extends App {
  val list = List(List(1, 2), List(4, 5))

  def sequence2[A](lo: List[List[A]]): List[List[A]] =
    lo.foldRight(List(List[A]())) { (opt, ol) =>
      for { o <- opt; l <- ol } yield o :: l
    }

  /*def sequence[A](lo: Monoid[Monoid[A]]): Monoid[Monoid[A]] =
    lo.foldRight (Monoid(Monoid[A].empty)) { (opt, ol) =>
      ol flatMap (l => opt map (o => o.combine(l)))
    }

  def mapM1[A](list:Monoid[A],f:A => Monoid[A]) = sequence(list.map(f))*/

  def mapM[A](list: List[A], f: A => List[A]) = sequence2(list.map(f))

  //  list.foldRight(List[List[Int]]())((a,b )=> a.map(y => List(y,b)) )
  def f(a: Char) = a match {
    case '0' => List('0', '1', '2', '3', '4')
    case _   => List('1')
  }

  val list1 = List('0', '0', '0', '0')

  println(list1.map(f))
  println(mapM(list1, f))

  def fib(n: Int): Int = {
    if (n <= 1) 1
    else fib(n - 1) + fib(n - 2)
  }

  val s = for (i <- 0 to 9) yield fib(i)
  println(s)
  println(s.sum)

}
