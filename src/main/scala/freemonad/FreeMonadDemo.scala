package freemonad

class FreeMonadDemo {
  def main(args: Array[String]): Unit = {
    Bind(GetUp, (_: Unit) =>
      Bind(Wash, (_: Unit) =>
        Bind(MakeBreakfast, (breakfast: Breakfast) =>
          Bind(Eat(breakfast), (_: Unit) =>
            Bind(GoOut, (_: Unit) =>
              Return(())))))): DoFree[GoToWork, Unit]




  }
}

//trait ~>[F[_],G[_]]{
//  def apply[A](fa:F[A]):G[A]
//}


case class Breakfast(egg: Int, milk: Int)

sealed trait GoToWork[A]

case object GetUp extends GoToWork[Unit]

case object Wash extends GoToWork[Unit]

case object MakeBreakfast extends GoToWork[Breakfast]

case class Eat(breakfast: Breakfast) extends GoToWork[Unit]

case object GoOut extends GoToWork[Unit]

trait DoFree[F[_], A] {
  def unit(a: A): DoFree[F, A] = Return(a)

  def flatMap[B](k: A => DoFree[F, B]): DoFree[F, B] = this match {
    case Return(a) => k(a)
    case Bind(command, cont) =>
      Bind(command, cont andThen (_ flatMap k))
  }

  def map[B](f: A => B): DoFree[F, B] =
    flatMap(f andThen (Return(_)))

  type FreeGoToWork[A] = DoFree[GoToWork,A]
  implicit def liftGoToWork[A](n:GoToWork[A]):FreeGoToWork[A] = DoFree.lift(n)
  val doo:FreeGoToWork[Unit] = for{
    _ <- GetUp
    _ <- Wash
    breakfast <- MakeBreakfast
    _ <- Eat(breakfast)
    _ <- GoOut
  } yield ()


//  def foldMap[G[_]: Monad](f: F ~> G): G[A] = {
//    val G = implicitly[Monad[G]]
//    this match {
//      case Return(a) =>
//        G.unit(a)
//      case Bind(command, cont) =>
//        G.flatMap(f(command))(cont andThen (_ foldMap f))
//    }
//  }

  type Id[A] = A
  //G必须提供Monad
  implicit val IdMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: A) = a
    def flatMap[A,B](m: Id[A])(k: A => Id[B]): Id[B] =
      m match { case a => k(a) }
  }



}
trait Functor[F[_]] {
  def map[A,B](a: F[A])(f: A => B): F[B]
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] = {
    flatMap(ma) { a => unit(f(a)) }
  }
}
object DoFree{
  def lift[F[_],R](command:F[R]):DoFree[F,R] =
    Bind(command,(x:R) => Return(x))
}

final case class Return[F[_], A](a: A) extends DoFree[F, A]

final case class Bind[F[_], R, A](command: F[R], cont: R => DoFree[F, A]) extends DoFree[F, A]