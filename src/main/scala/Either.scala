sealed trait Either[+E, +A] {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if (xs.isEmpty)
      Left("mean of empty list")
    else
      Right(xs.sum / xs.length)
  }

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  def map[B](f:A => B):Either[E,B] = this match {
    case Right(a) => Right(f(a))
    case Left(a) => Left(a)
  }
  def flatMap[EE >: E,B](f : A => Either[EE,B]) : Either[EE,B] = this match {
    case Right(a) => f(a)
    case Left(a) => Left(a)
  }

//  def orElse[EE >: E,B >: A](b: => Either[EE,B]) : Either[EE,B] = this match {
//    case Right(a) => Right(a)
//    case Left(_) => b
//  }
//
//  def map2[EE >: E,B,C](b:Either[EE,B])(f:(A,B) => C):Either[EE,C] = for{
//    a <- this
//    bb <- b
//  } yield f(a,bb)
//
//  def sequence[E,A](es:List[Either[E,A]]):Either[E,List[A]] = for {
//    a <- es
//  } yield Try(a)
//
//  def traverse[E,A,B](as:List[A])(f:A => Either[E,B]):Either[E,List[B]] = as match {
//    case Cons(x,xs) => (f(x) map2 traverse(xs)(f))(Cons(_,_))
//    case Nil => Right(Nil)
//  }




}
object Either{
  def main(args: Array[String]): Unit = {

  }
}
case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]
