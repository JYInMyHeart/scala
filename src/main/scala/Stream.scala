


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
      if (!p(h())) Stream.cons[A](h(), t().takeWhile(p))
      else t().takeWhile(p)
  }

  def exists(p: A => Boolean): Boolean = this match {
    case ConsS(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldr[B](z: => B)(f: (A, => B) => B): B = this match {
    case ConsS(h, t) => f(h(), t().foldr(z)(f))
    case _ => z
  }

  def existsByFoldr(p: A => Boolean): Boolean =
    foldr(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldr(true)((a, b) => p(a) && b)

  def takeWhileByFoldr(p: A => Boolean): Stream[A] =
    foldr(Stream[A]())((a,b) => if(!p(a)) Stream.cons(a, b) else b)

  def headByFoldr: Option[A] =
    foldr(Option.empty[A])((a,_) => Some(a))


  def map[B](f:A => B):Stream[B] =
    foldr(Stream.empty[B])((a,b) => Stream.cons[B]( f(a), b))

  def filter(p:A => Boolean):Stream[A] =
    foldr(Stream.empty[A])((a,b) => if(p(a)) Stream.cons( a, b) else b)

  def append[B >: A](aa: Stream[B]):Stream[B] =
    foldr(aa)((a,b) => Stream.cons(a,b))

  def flatMap[B](f:A => Stream[B]):Stream[B] =
    foldr(Stream.empty[B])((a,b) => f(a).append(b))

  def find(p:A => Boolean):Option[A] =
    filter(p).headByFoldr

  def constant[A](a:A):Stream[A] =
    Stream.cons(a,constant(a))

  def from(n:Int):Stream[Int] =
    Stream.cons(n,from(n + 1))

  def fibs(n:Int, m:Int):Stream[Int] =
    Stream.cons(n,fibs(m,n + m))

  def unfold[A,S](z:S)(f:S => Option[(A,S)]):Stream[A] =
   f(z) match {
     case Some(a) =>Stream.cons(a._1,unfold(a._2)(f))
     case _ => Stream.empty[A]
   }

  def fibsByUnfold(n:Int, m:Int) :Stream[Int] =
    unfold(n)(n => Some((n + m,m)))


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
    println(Stream( 2, 3).headOption)
    println(Stream( 2, 3).headByFoldr)
    println(Stream(1, 2, 3, 4, 5, 6).takeWhile(_ > 4).toList)
    println(Stream(1, 2, 3, 4, 5, 6).takeWhileByFoldr(_ > 4).toList)
    println(Stream(1, 2, 3, 4).exists(_ > 3))
    println(Stream(1,2,3,4).forAll(_ >= 1))
    println(Stream(1,2,3,4).map(_ + 1).toList)
    println(Stream(1,2,3,4).flatMap(x => Stream(x + 1)).toList)
    println(Stream(1,2,3,4).append(Stream(5)).toList)
    println(Stream(1,2,3).find(_ > 1))
    println(Stream().constant(2).take(3))
    println(Stream().from(2).take(3))
    println(Stream().fibs(1,1).take(10))
    println(Stream().fibs(1,1).take(10))
  }


}




