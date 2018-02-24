


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A,tail:List[A]) extends List[A]

object List{
  def sum(ints:List[Int]) :Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]) : Double = ds match {
    case Nil => 1.0
    case Cons(0.0,_) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as:A* ) : List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head,apply(as.tail:_*))
  }

  def tail[A](list: List[A]) : List[A] = list match {
    case Nil => Nil
    case Cons(x,xs) => xs
  }

  def setHead[A](value:A,list:List[A]) : List[A] = list match {
    case Nil => Nil
    case Cons(x,xs) => Cons(value,xs)
  }

  def drop[A](list: List[A],n:Int) : List[A] = {
    if(n > 0) drop(tail(list),n - 1)
    else if(n == 0) list
    else Nil
  }

  def dropWhile[A](list:List[A],f:A => Boolean):List[A] = list match {
    case Nil => Nil
    case Cons(x,xs) => if (f(x)) Cons(x,dropWhile(xs,f)) else dropWhile(xs,f)
  }

  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(x,xs) => if(xs != Nil && tail(xs) == Nil) Cons(x,Nil) else Cons(x,init(xs))
  }


  def main(args: Array[String]): Unit = {
    val ex1:List[Double] = Nil
    val ex2:List[Int] = Cons(1,Nil)
    val ex3:List[Int] = Cons(2,ex2)
    val x = List(1,2,3,4,5) match {
      case Cons(x,Cons(2,Cons(4,_))) => x
      case Nil => 42
      case Cons(x,Cons(y,Cons(3,Cons(4,_)))) => x + y
      case Cons(h,t) => h + sum(t)
      case _ => 101
    }
    def f(a:Int) = a > 2
    println(init(List(1,2,3,4)))
  }



}


