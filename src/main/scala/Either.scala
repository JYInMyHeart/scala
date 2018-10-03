import MyList.{Cons, List}

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

  def Try[A](a: => A): Either[E, A] =
    try Right(a)
    catch {
      case e: E => Left(e)
    }

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(a) => Left(a)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(a) => Left(a)
  }

  def opTop[E, B](either: => Either[E, B]): B = either match {
    case Right(x) => x
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    a <- this
    bb <- b
  } yield f(a, bb)


  def sequence[EE >: E, A](es: List[Either[EE, A]]): Either[EE, List[A]] =
    Try(List.map(es)(opTop(_)))

  def traverse[EE >: E, A, B](as: List[A])(f: A => Either[EE, B]): Either[EE, List[B]] =
    Try(List.map(as)(i => opTop( f(i))))


}

object Either {
  def main(args: Array[String]): Unit = {

  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]
