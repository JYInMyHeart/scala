package hs

import scalaz._
import hs.DT.{f1, f2}
import hs.Inner.{Foo, Unwrap}

import scala.concurrent.{ExecutionContext, Future}

class DT {}
object DT extends App {
  trait DepValue {
    type V
    val value: V
  }

  def magic(that: DepValue): that.V = that.value

  def mk[T](x: T) = new DepValue {
    override type V = T
    override val value = x
  }

  trait Foo {
    class Bar
    def doNothing(b: Bar) {}
  }

  val f1 = new Foo {}
  val b1: f1.Bar = new f1.Bar
  val f2 = new Foo {}
  val b2: f2.Bar = new f2.Bar
  f1.doNothing(b1)
  f2.doNothing(b2)
  val depInt = mk(1)
  val depString = mk("a")
  val itWorks = magic(depInt)
  val again = magic(depString)
  println(itWorks)
  println(again)

  println(magic(mk(1)))
  println(magic(mk("a")))

  trait Inner[F] {
    type T
  }
  object Inner {
    def apply[F](implicit inner: Inner[F]) = inner
    implicit def mk[F[_], A] = new Inner[F[A]] {
      type T = A
    }
  }
}
trait IsFuture[F] {
  type T

  def apply(f: F): Future[T]

}
object IsFuture {
  def apply[F](implicit isf: IsFuture[F]) = isf
  implicit def mk[A] = new IsFuture[Future[A]] {
    type T = A

    override def apply(f: Future[A]): Future[A] = f

  }

  implicit def e = new ExecutionContext {
    override def execute(runnable: Runnable): Unit =
      new Thread(runnable).start()
    override def reportFailure(cause: Throwable): Unit = println(cause)
  }

  def logResult[Thing](
    thing: Thing
  )(implicit isf: IsFuture[Thing]): Future[isf.T] =
    isf(thing) map { x =>
      println(s"got $x")
      x
    }

  def main(args: Array[String]): Unit = {
    IsFuture.logResult(Future("sb"))
    IsFuture.logResult(Future(1))
  }

}

trait Apart[F] {
  type T
  type W[X]

  def apply(f: F): W[T]
}

object Apart {
  def apply[F](implicit apart: Apart[F]) = apart

  implicit def mk[F[_], A] = new Apart[F[A]] {
    type T = A
    type W[X] = F[X]

    def apply(f: F[A]): W[T] = f
  }

  type Aux[FA, A, F[_]] = Apart[FA] {
    type T = A
    type W[X] = F[X]
  }

  implicit def mk1[F[_], A]: Aux[F[A], A, F] = new Apart[F[A]] {
    type T = A
    type W[X] = F[X]
    def apply(f: F[A]): W[T] = f
  }

}

trait Demo[F[_]] {
  type W[X] = F[X]
  type Ignore[X] = F[Int]
  type Identity[X] = X
  type Const[X] = Int
}

object ApplyEither {
  def apply[T, F, G](t: T, f: F, g: G)(implicit ea: EApply[T, F, G]): ea.Out =
    ea(t, f, g)
}

trait EApply[T, F, G] {
  type Out
  def apply(t: T, f: F, g: G): Out
}
object EApply extends LowPriorityEApply {
  def apply[T, F, G](implicit ea: EApply[T, F, G]) = ea

  implicit def fapply[T, R, G] = new EApply[T, T => R, G] {
    type Out = R
    def apply(t: T, f: T => R, g: G) = f(t)
  }

  def main(args: Array[String]): Unit = {
    val out = ApplyEither(1, { x: Int =>
      42
    }, { x: Double =>
      "no"
    })
    println(out)
  }
}
trait LowPriorityEApply {
  implicit def gapply[T, R, F] = new EApply[T, F, T => R] {
    type Out = R
    def apply(t: T, f: F, g: T => R) = g(t)
  }
}

object ApplyMany {
  def apply[A, B, C, D](a: A, b: B, c: C, d: D)(
    implicit xa: XApply[A, B, C, D]
  ) = xa(a, b, c, d)
}

trait XApply[A, B, C, D] {
  type Out
  def apply(a: A, b: B, c: C, d: D): Out
}

object XApply {
  def apply[A, B, C, D](implicit xa: XApply[A, B, C, D]) = xa

  implicit def bapply[A, B, C, D] = new XApply[A, A => B, C, D] {
    override type Out = B
    override def apply(a: A, b: A => B, c: C, d: D): B = b(a)
  }

  implicit def dapply[A, B, C, D] = new XApply[A, B, C, A => D] {
    override type Out = D
    override def apply(a: A, b: B, c: C, d: A => D): D = d(a)
  }

  implicit def capply[A, B, C, D] = new XApply[A, B, A => C, D] {
    override type Out = C
    override def apply(a: A, b: B, c: A => C, d: D): C = c(a)
  }
  def main(args: Array[String]): Unit = {
    val out = ApplyMany(false, { z: Boolean =>
      !z
    }, { x: Int =>
      x + 1
    }, { y: String =>
      y + " sb"
    })

    val out1 = ApplyMany("a", { z: Boolean =>
      !z
    }, { x: Int =>
      x + 1
    }, { y: String =>
      y + " sb"
    })

    val out2 = ApplyMany(3, { z: Boolean =>
      !z
    }, { x: Int =>
      x + 1
    }, { y: String =>
      y + " sb"
    })

//    val out3 = ApplyMany('a', { z: Boolean =>
//      !z
//    }, { x: Int =>
//      x + 1
//    }, { y: String =>
//      y + " sb"
//    })
    println(out)
    println(out1)
    println(out2)
  }

}

trait Inner[F] {
  type T
}
object Inner {
  def apply[F](implicit inner: Inner[F]) = inner
  implicit def mk[F[_], A] = new Inner[F[A]] {
    type T = A
  }

  case class Foo[V](value: V)
  trait Unwrap[F] {
    type Inner
  }

//  def zero[T, A](t: T)(m: Monoid[A])(implicit inner: Inner.Aux[T, A]): inner.T =
//    m.zero

  def main(args: Array[String]): Unit = {
    implicit def e = new ExecutionContext {
      override def execute(runnable: Runnable): Unit =
        new Thread(runnable).start()
      override def reportFailure(cause: Throwable): Unit = println(cause)
    }

    def annoy[A](that: Future[List[Set[Int]]],
                 f: Int => A): Future[List[Set[A]]] =
      that map (_ map (_ map f))

  }

}
object Unwrap {
  def apply[F](implicit unwrap: Unwrap[F]) = unwrap

  type Aux[T, A] = Unwrap[T] { type Inner = A }

  implicit def nested[F[_], G](implicit unwrap: Unwrap[G]) =
    new Unwrap[F[G]] {
      type Inner = unwrap.Inner
    }

  implicit def bottom[F[_], A] =
    new Unwrap[F[A]] {
      type Inner = A
    }
  def zero[T, A](t: T)(m: Monoid[A])(
    implicit unwrap: Unwrap.Aux[T, A]
  ): unwrap.Inner = m.zero

  def main(args: Array[String]): Unit = {
    val int = new Monoid[Int] {
      override def zero: Int = 0
      override def add(m1: Int, m2: Int): Int = m1 + m2
    }
    val out = zero(Foo(Foo(1)))(int)
    val out1 = zero(Foo(1))(int)
    assert(out == 0)
    println(out1)
  }
}

object MapIt {
  def apply[A, B, C](in: A, f: B => C)(
    implicit mapper: Mapper[A, B, C]
  ): mapper.Out = mapper(in, f)
}

trait Mapper[A, B, C] {
  type Out
  def apply(a: A, f: B => C): Out
}
object Mapper extends App {
  def apply[A, B, C](implicit mapper: Mapper[A, B, C]) = mapper
  type Aux[A, B, C, Out0] = Mapper[A, B, C] { type Out = Out0 }
  implicit val f = new Functor[List] {
    override def map[A, B](a: List[A])(f: A => B): List[B] = a map f
  }

  implicit def base[F[_], A, B >: A, C](
    implicit f: Functor[F]
  ): Aux[F[A], B, C, F[C]] =
    new Mapper[F[A], B, C] {
      type Out = F[C]
      override def apply(a: F[A], g: B => C): F[C] = f.map(a)(g)
    }
  implicit def recur[F[_], A, B, C](
    implicit nested: Mapper[A, B, C],
    f: Functor[F]
  ): Aux[F[A], B, C, F[nested.Out]] =
    new Mapper[F[A], B, C] {
      type Out = F[nested.Out]
      override def apply(a: F[A], g: B => C): F[nested.Out] =
        f.map(a)(nested(_, g))
    }

  val in = List(List(List(1)))
  val in1 = List(List("a"))
  val out = MapIt(in, (x: Int) => x + 1)
  val out1 = MapIt(in1, (x: String) => x + "a")
  assert(List(List(List(2))) == out)
  assert(List(List("aa")) == out1)
}
