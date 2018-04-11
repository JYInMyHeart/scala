sealed trait Stream[+A]

case object Empty extends Stream[Nothing]

case class ConsS[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    ConsS(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

//  def headOption[A]: Option[A] = this match {
//    case Empty => None
//    case ConsS(h, t) => Some(h())
//  }

//  def exists(p: A => Boolean):Boolean = this match {
//    case ConsS(h,t) => p(h()) || t().exists(p)
//    case _ = false
//  }
}

