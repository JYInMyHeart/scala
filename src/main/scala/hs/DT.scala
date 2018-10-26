package hs

import scala.concurrent.Future

class DT {


}
object DT extends App{
  trait DepValue{
    type V
    val value :V
  }

  def magic(that:DepValue):that.V = that.value

  def mk[T](x:T) = new DepValue {
    override type V = T
    override val value = x
  }

  trait Foo{
    class Bar
    def doNothing(b:Bar){}
  }

  val f1 = new Foo {}
  val b1 = new f1.Bar
  val f2 = new Foo {}
  val b2 = new f2.Bar
  f1.doNothing(b1)
  f2.doNothing(b2)
  val depInt = mk(1)
  val depString = mk("a")
  val itWorks = magic(depInt)
  val again = magic(depString)
  println(itWorks)
  println(again)

  trait Inner[F]{
    type T
  }
  object Inner{
    def apply[F](implicit inner: Inner[F]) = inner
    implicit def mk[F[_],A] = new Inner[F[A]] {
      type T = A
    }
  }

  trait IsFuture[F]{
    type T

    def apply(f:F):Future[T]
  }
  object IsFuture{
    def apply[F](implicit isf:IsFuture[F]) = isf
    implicit def mk[A] = new IsFuture[Future[A]] {
      type T = A

      override def apply(f: Future[A]): Future[A] = f
    }
  }

  def logResult[Thing](thing: Thing) = {

  }


}
