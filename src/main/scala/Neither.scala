import MyList.{Cons, List}

import scala.language.higherKinds

sealed trait NEither[+E, +A] {
//  def mean(xs: IndexedSeq[Double]): NEither[String, Double] = {
//    if (xs.isEmpty)
//      NLeft("mean of empty list")
//    else
//      NRight(xs.sum / xs.length)
//  }
//
//  def safeDiv(x: Int, y: Int): NEither[Exception, Int] =
//    try NRight(x / y)
//    catch {
//      case e: Exception => NLeft(e)
//    }
//
  def Try[A](a: => A): NEither[List[E], A] =
    try NRight(a)
    catch {
      case e: E => NLeft(List(e))
    }
//
//  def map[B](f: A => B): NEither[E, B] = this match {
//    case NRight(a) => NRight(f(a))
//    case NLeft(a) => NLeft(a)
//  }

//  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
//    case Right(a) => f(a)
//    case Left(a) => Left(a)
//  }
//  def opTop[E, B](either: => NEither[E, B]): B = either match {
//    case NRight(x) => x
//  }
//
//  def orElse[EE >: E, B >: A](b: => NEither[EE, B]): NEither[EE, B] = this match {
//    case NRight(a) => NRight(a)
//    case NLeft(_) => b
//  }
//
//  def map2[EE >: E, B, C](b: NEither[List[EE], B])(f: (A, B) => C): NEither[List[EE], C] = for {
//    a <- this
//    bb <- b
//  } yield f(a, bb)
//
//
//  def sequence[EE >: E, A](es: List[NEither[EE, A]]): NEither[EE, List[A]] =
//    Try(List.map(es)(opTop(_)))
//
//  def traverse[EE >: E, A, B](as: List[A])(f: A => NEither[EE, B]): NEither[EE, List[B]] =
//    Try(List.map(as)(i => opTop( f(i))))


}

object NEither {
  def main(args: Array[String]): Unit = {

  }
}

case class NLeft[+E](value: List[E]) extends NEither[List[E], Nothing]

case class NRight[+A](value: A) extends NEither[Nothing, A]
