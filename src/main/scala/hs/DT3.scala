package hs
import java.util.concurrent.Executor

import scalaz.{Functor, NonEmptyList}

import scala.concurrent.{ExecutionContext, Future}

class DT3 {}

trait IsFuture1[F] {
  type T
  def apply(f: F): Future[T]
}

object IsFuture1 {
  def apply[F](implicit isf: IsFuture1[F]) = isf

  implicit def mk[A] = new IsFuture1[Future[A]] {
    type T = A
    override def apply(f: Future[A]): Future[A] = f
  }

  implicit def executor: ExecutionContext = new ExecutionContext {
    override def execute(runnable: Runnable): Unit =
      runnable.run()
    override def reportFailure(cause: Throwable): Unit = println(cause)
  }

  def log[Thing](thing: Thing)(implicit isFuture: IsFuture1[Thing]) =
    isFuture(thing) map { x =>
      println(s"I got $x")
      x
    }

  def main(args: Array[String]): Unit = {
    println(log(Future(1)))
  }
}
