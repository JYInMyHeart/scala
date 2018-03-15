
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](value: A, list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(x, xs) => Cons(value, xs)
  }

  def drop[A](list: List[A], n: Int): List[A] = {
    if (n > 0) drop(tail(list), n - 1)
    else if (n == 0) list
    else Nil
  }

  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f)) else dropWhile(xs, f)
  }

  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(x, xs) => if (xs != Nil && tail(xs) == Nil) Cons(x, Nil) else Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def fold[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldLeft(as, z)(f)

  def reserve[A](as: List[A]): List[A] = foldLeft(as, List[A]())((x, y) => Cons(y, x))

//  def fistElement[A](as: List[A]): Option[A] = as match {
//    case Cons(x, xs) => Option(x)
//    case Nil => None
//  }

  def append[A](as: List[A], a: A): List[A] = nFoldRight(as, List(a))((x, y) => Cons(x, y))

  def appendList[A](as: List[A], bs: List[A]): List[A] = foldLeft(bs, as)((x, y) => append(x, y))

  def nFoldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reserve(as), z)((b, a) => f(a, b))

  def nFoldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(reserve(as), z)((b, a) => f(a, b))

  def ince(ints: List[Int]): List[Int] = reserve(foldLeft(ints, List[Int]())((x, y) => Cons(y + 1, x)))

  def doubleToString(strings: List[Double]): List[String] = foldLeft(reserve(strings), List[String]())((x, y) => Cons(y.toString.concat("s"), x))

  def map[A, B](as: List[A])(f: A => B): List[B] = foldLeft(reserve(as), List[B]())((x, y) => Cons(f(y), x))

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldLeft(reserve(as), List[B]())((x, y) => appendList(f(y), x))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldLeft(reserve(as), List[A]())((x, y) => if (f(y)) Cons(y, x) else x)

  def filterByFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if (f(x)) List(x) else List())

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a,b) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(h1,t1),Cons(h2,t2)) => Cons(f(h1,h2),zipWith(t1,t2)(f))
  }


  def main(args: Array[String]): Unit = {
//    val ex1: List[Double] = Nil
//    val ex2: List[Int] = Cons(1, Nil)
//    val ex3: List[Int] = Cons(2, ex2)
//    val x = List(1, 2, 3, 4, 5) match {
//      case Cons(x, Cons(2, Cons(4, _))) => x
//      case Nil => 42
//      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
//      case Cons(h, t) => h + sum(t)
//      case _ => 101
//    }
//
//    def f(a: Int) = a > 2
//
//    println(init(List(1, 2, 3, 4)))
//    println(appendList(List(1, 2), List(3, 4)))
//    println(ince(List(1, 2, 3)))
//    println(doubleToString(List(0.1, 0.2, 0.3)))
//    println(map(filter(List(1, 2, 3))(x => x > 1))(x => x + 1))
//    println(map(filterByFlatMap(List(1, 2, 3))(x => x > 1))(x => x + 1))
    println(zipWith(List(1, 2), List(4, 5))((x, y) => x + y))



  }


}


