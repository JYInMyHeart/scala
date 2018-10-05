


sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case ConsS(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case ConsS(h, t) => h() :: t().toList
  }

  def take(n: Int): List[A] = this match {
    case Empty => Nil
    case ConsS(h, t) =>
      if (n > 0) h() :: t().take(n - 1)
      else Nil
  }

  def drop(n: Int): Option[Stream[A]] = this match {
    case Empty => None
    case ConsS(h, t) =>
      if (n > 0) t().drop(n - 1)
      else Some(t())
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case ConsS(h, t) =>
      if (!p(h())) ConsS(h, () => t().takeWhile(p))
      else t().takeWhile(p)
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Empty => false
    case ConsS(h, t) => p(h()) || t().exists(p)
  }
}

case object Empty extends Stream[Nothing]

case class ConsS[+A](h: () => A,
                     t: () => Stream[A]) extends Stream[A]


object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    ConsS(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))


  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3).take(2))
    println(Stream(1, 2, 3).headOption)
    println(Stream(1, 2, 3, 4, 5, 6).takeWhile(_ > 4).toList)
    println(Stream(1,2,3,4).exists(_ > 3))
  }


}




