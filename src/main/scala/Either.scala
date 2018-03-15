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

  def map[B](f:A => B):Either[E,B] = {
    try Right(f(this))
    catch {
      case e: Exception => Left(new E(e))
    }
  }
  def flatMap[EE >: E,B](f : A => Either[EE,B]) : Either[EE,B] = {
    try f(this)
    catch {
      case e: Exception => Left(new EE(e))
    }
  }

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]
