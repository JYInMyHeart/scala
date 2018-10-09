import Stream._

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case ConsS(h, _) => Some(h())
  }

  def toListRecursive: List[A] = this match {
    case ConsS(h, t) => h() :: t().toListRecursive
    case _ => Nil
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case ConsS(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }

  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]

    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case ConsS(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }

    go(this)
  }

  def takeToList(n: Int): List[A] = this match {
    case Empty => Nil
    case ConsS(h, t) =>
      if (n > 0) h() :: t().takeToList(n - 1)
      else Nil
  }

  def take(n: Int): Stream[A] = this match {
    case ConsS(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case ConsS(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case ConsS(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case ConsS(h, t) if p(h()) =>
      cons(h(), t().takeWhile(p))
    case _ => empty
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
    foldr(Stream[A]())((a, b) => if (p(a)) cons(a, b) else empty)

  def headByFoldr: Option[A] =
    foldr(None:Option[A])((a, _) => Some(a))


  def map[B](f: A => B): Stream[B] =
    foldr(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldr(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else b)

  def append[B >: A](aa: => Stream[B]): Stream[B] =
    foldr(aa)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldr(Stream.empty[B])((a, b) => f(a).append(b))

  def find(p: A => Boolean): Option[A] =
    filter(p).headByFoldr

  def constant[B >: A](a: B): Stream[B] ={
    lazy val tail = cons(a, constant(a))
    tail
  }


  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs(n: Int, m: Int): Stream[Int] =
    cons(n, fibs(m, n + m))

  def unfold[ A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a,b)) => cons(a, unfold(b)(f))
      case _ => empty
    }

  def fibsByUnfold(n: Int, m: Int): Stream[Int] =
    unfold(n)(n => Some((n + m, m)))

  def mapByUnfold[B](f: A => B): Stream[B] =
    unfold(this){
      case ConsS(h, t) => Some((f(h()),t()))
      case _ => None
    }

  def takeByUnfold(n:Int):Stream[A] =
    unfold(this,n){
      case (ConsS(h, t),n) if n > 0 =>
        Some((h(),(t(),n - 1)))
      case _ => None
    }

  def takeWhileByUnfold(p: A => Boolean): Stream[A] =
    unfold(this,p){
      case (ConsS(h,t),f) if p(h()) => Some((h(),(t(),f)))
      case _ => None
    }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold(this,s2){
      case (ConsS(h,t),ConsS(h1,t1)) =>
        Some( (f(h(),h1()),(t(),t1())) )
      case _ => None
    }

  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  def zipWithAll[B,C](s2:Stream[B])(f: (Option[A], Option[B]) => C):Stream[C] =
    unfold(this,s2){
      case (Empty, Empty) => None
      case (ConsS(h, t), Empty) =>
        Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, ConsS(h, t)) =>
        Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (ConsS(h1, t1), ConsS(h2, t2)) =>
        Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  def startWith[B](s2:Stream[B]):Boolean =
    zipAll(s2).takeWhile(_._2 != None) forAll{
      case (h1,h2) => h1 == h2
    }

  def tails:Stream[Stream[A]] =
    unfold(this){
      case ConsS(h, t) =>
        Some((cons[A](h(),t()),t()))
      case _ => None
    } append Stream(empty)

  def hasSubsequence[A](s:Stream[A]):Boolean =
    tails exists (_ startWith s)

  //some wrong implementation
  def scanRight1[B](z:B)(f:(A, => B) => B):Stream[B] =
    unfold(this){
      case ConsS(h, t) =>
        Some((f(h(),z),t()))
      case _ => Some((z,empty))
    }

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldr((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2


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
    println(Stream(1, 2, 3).take(2).toListFast)
    println(Stream(2, 3).headOption)
    println(Stream(2, 3).headByFoldr)
    println(Stream(1, 2, 3, 4, 5, 6).takeWhile(_ > 4).toList)
    println(Stream(1, 2, 3, 4, 5, 6).takeWhileByFoldr(_ > 4).toList)
    println(Stream(1, 2, 3, 4).exists(_ > 3))
    println(Stream(1, 2, 3, 4).forAll(_ >= 1))
    println(Stream(1, 2, 3, 4).map(_ + 1).toList)
    println(Stream(1, 2, 3, 4).mapByUnfold(_ + 1).toList)
    println(Stream(1, 2, 3, 4).flatMap(x => Stream(x + 1)).toList)
    println(Stream(1, 2, 3, 4).append(Stream(5)).toList)
    println(Stream(1, 2, 3).find(_ > 1))
    println(Stream().constant(2).take(3).toListFast)
    println(Stream().from(2).take(3).toListFast)
    println(Stream().fibs(1, 1).take(10).toListFast)
    println(Stream().fibs(1, 1).take(10).toListFast)

    println(Stream(1, 2, 3).drop(0).toList)
    println(Stream(1, 2, 3).takeByUnfold(1).toList)
    println(Stream(1, 2, 3).zip(Stream(4,5,6,7)).toList)
    println(Stream(1, 2, 3).zipAll(Stream(4,5,6,7)).toList)

    println(Stream(1,2,3,4,5) startWith Stream(1,2,4))
    println(Stream(1,2,3).tails.map(_.toListFast).toList)
    println(Stream(1,2,3).scanRight(0)(_+_).toList)
  
    println(Stream(1,2,3).scanRight1(0)(_+_).toList)
  }


}




