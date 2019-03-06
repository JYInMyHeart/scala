package catsLearn

import catsLearn.LT.<
import catsLearn.LTEQ.<=

import scala.collection.mutable

class TypeClassLearn {
  def combineAll[A](list: List[A], A: Monoid[A]): A =
    list.foldRight(A.empty)(A.combine)

  def combineAll1[A <: Monoid[A]](list: List[A]): A = ???
}

abstract final case class Pair[A, B](first: A, second: B)
    extends Monoid[Pair[A, B]] {
  def empty(implicit eva: A <:< Monoid[A], evb: A <:< Monoid[B]): Pair[A, B] =
    ???
}

trait Monoid[A] {
  def empty: A

  def combine(x: A, y: A): A
}

trait Pet[A] {
  def name(a: A): String

  def renamed(a: A, newName: String): A
}

case class Fish(name: String, age: Int)

object Fish {

  implicit class RenameOps[A](a: A)(implicit ev: Pet[A]) {
    def renamed(newName: String): A = ev.renamed(a, newName)
  }

  implicit val FishPet: Pet[Fish] = new Pet[Fish] {
    override def name(a: Fish): String = a.name

    override def renamed(a: Fish, newName: String): Fish =
      a.copy(name = newName)
  }
}

//case class Kitty(name:String,color:Color) extends Pet[Kitty]{
//  override def renamed(newName: String): Kitty = Kitty(newName,Color.ALICEBLUE)
//}

trait Animal {
  def name: String
}

trait Rename[A] {
  def rename(a: A, newName: String): A
}

case class Dog(name: String, age: Int) extends Animal

object Dog {

  implicit class RenameOps[A](a: A)(implicit ev: Rename[A]) {
    def renamed(newName: String): A = ev.rename(a, newName)
  }

  implicit val DogRename: Rename[Dog] = (a: Dog, newName: String) =>
    a.copy(newName)
}

object Pet extends App {

  //  def esquire[A <: Pet[A]](a:A):A = a.renamed(a.name + ", Esq")
  //  val a1 = Fish("Jimmy",2)
  //  val v = a1.renamed("Bob")
  ////  val a = Dog("J",4)
  ////  val b = a.renamed("s")
  //  println(v)

  val q = mutable.Queue[P]()

  case class P(name: String, age: Int)

  val a = List(P("a", 1), P("b", 2), P("c", 3), P("d", 2))
  q ++= a
  val d = q.find(_.age == 2).get
  println(q)
  println(d)
}

sealed trait Nat

final class _0 extends Nat

final class Succ[P <: Nat] extends Nat

trait Sum[A <: Nat, B <: Nat] {
  type Out <: Nat
}

object Sum {
  def apply[A <: Nat, B <: Nat](implicit sum: Sum[A, B]): Aux[A, B, sum.Out] =
    sum

  type Aux[A <: Nat, B <: Nat, C <: Nat] = Sum[A, B] { type Out = C }

  implicit def sum1[B <: Nat]: Aux[_0, B, B] = new Sum[_0, B] {
    type Out = B
  }

  implicit def sum2[A <: Nat, B <: Nat](
      implicit sum: Sum[A, Succ[B]]): Aux[Succ[A], B, sum.Out] =
    new Sum[Succ[A], B] {
      type Out = sum.Out
    }

  def main(args: Array[String]): Unit = {
    Sum[Succ[Succ[Succ[_0]]], Succ[_0]]
  }
}

trait LT[A <: Nat, B <: Nat]

object LT {
  def apply[A <: Nat, B <: Nat](implicit lt: A < B): LT[A, B] = lt

  type <[A <: Nat, B <: Nat] = LT[A, B]

  implicit def lt1[B <: Nat]: <[_0, Succ[B]] = new <[_0, Succ[B]] {}

  implicit def lt2[A <: Nat, B <: Nat](
      implicit lt: A < B): <[Succ[A], Succ[B]] = new LT[Succ[A], Succ[B]] {}

  def main(args: Array[String]): Unit = {
    type _1 = Succ[_0]
    type _2 = Succ[_1]
    LT[_0, _1]
    LT[_0, _2]
  }
}

trait LTEQ[A <: Nat, B <: Nat]

object LTEQ {
  def apply[A <: Nat, B <: Nat](implicit lteq: A <= B): LTEQ[A, B] = lteq

  type <=[A <: Nat, B <: Nat] = LTEQ[A, B]

  implicit def lteq1 = new <=[_0, _0] {}

  implicit def lteq2[A <: Nat] = new <=[_0, Succ[A]] {}

  implicit def lteq3[A <: Nat, B <: Nat](implicit lteq: A <= B) =
    new <=[Succ[A], Succ[B]] {}

  def main(args: Array[String]): Unit = {
    type _1 = Succ[_0]
    type _2 = Succ[_1]
    LTEQ[_0, _1]
    LTEQ[_0, _0]
    LTEQ[_1, _2]
    LTEQ[_1, _1]
  }
}

trait EQ[A <: Nat, B <: Nat]

object EQ {
  def apply[A <: Nat, B <: Nat](implicit lteq: A === B): EQ[A, B] = lteq

  type ===[A <: Nat, B <: Nat] = EQ[A, B]

  implicit def eq1 = new ===[_0, _0] {}

  implicit def eq2[A <: Nat, B <: Nat](implicit eq: A === B) =
    new ===[Succ[B], Succ[A]] {}

  def main(args: Array[String]): Unit = {
    type _1 = Succ[_0]
    type _2 = Succ[_1]
    EQ[_0, _0]
    EQ[_1, _1]
  }
}

sealed trait HList

final class ::[+H, +T <: HList] extends HList

final class HNil extends HList

trait LTEqs[H <: HList, A <: Nat] {
  type Out <: HList
}

object LTEqs {

  def apply[A <: HList, B <: Nat](
      implicit lteqs: LTEqs[A, B]): Aux[A, B, lteqs.Out] = lteqs

  type Aux[A <: HList, B <: Nat, C <: HList] = LTEqs[A, B] { type Out = C }

  implicit def hnilLTEqs[A <: Nat]: Aux[HNil, A, HNil] = new LTEqs[HNil, A] {
    type Out = HNil
  }

  implicit def hlistLTEqsLower[A <: Nat, H <: Nat, T <: HList](
      implicit lts: LTEqs[T, A],
      l: H <= A): LTEqs[H :: T, A] {
    type Out = H :: lts.Out
  } = new LTEqs[H :: T, A] {
    type Out = H :: lts.Out
  }

  implicit def hlistLTEqsLower[A <: Nat, H <: Nat, T <: HList](
      implicit lts: LTEqs[T, A],
      l: A < H): LTEqs[H :: T, A] {
    type Out = lts.Out
  } = new LTEqs[H :: T, A] {
    type Out = lts.Out
  }

}

trait GTs[H <: HList, A <: Nat] {
  type Out <: HList
}

object GTs {

  import LT._
  import LTEQ._

  type Aux[H <: HList, A <: Nat, Out0 <: HList] = GTs[H, A] { type Out = Out0 }

  def apply[H <: HList, A <: Nat](implicit lts: GTs[H, A]): Aux[H, A, lts.Out] =
    lts

  implicit def hnilGTEqs[A <: Nat]: Aux[HNil, A, HNil] = new GTs[HNil, A] {
    type Out = HNil
  }

  implicit def hlistGTEqsLower[A <: Nat, H <: Nat, T <: HList](
      implicit lts: GTs[T, A],
      l: A < H): Aux[H :: T, A, H :: lts.Out] =
    new GTs[H :: T, A] {
      type Out = H :: lts.Out
    }

  implicit def hlistGTEqsGreater[A <: Nat, H <: Nat, T <: HList](
      implicit lts: GTs[T, A],
      l: H <= A): Aux[H :: T, A, lts.Out] =
    new GTs[H :: T, A] {
      type Out = lts.Out
    }
}

trait Prepend[P <: HList, S <: HList] {
  type Out <: HList
}

object Prepend {
  type Aux[P <: HList, S <: HList, Out0 <: HList] = Prepend[P, S] {
    type Out = Out0
  }

  def apply[P <: HList, S <: HList](
      implicit prepend: Prepend[P, S]): Aux[P, S, prepend.Out] = prepend

  implicit def hnilPrepend1[P <: HNil, S <: HList]: Aux[P, S, S] =
    new Prepend[P, S] {
      type Out = S
    }

  implicit def hlistPrepend[P, PT <: HList, S <: HList](
      implicit pt: Prepend[PT, S]): Aux[P :: PT, S, P :: pt.Out] =
    new Prepend[P :: PT, S] {
      type Out = P :: pt.Out
    }

  def main(args: Array[String]): Unit = {
    type _1 = Succ[_0]
    type _2 = Succ[_1]
    type _3 = Succ[_2]
    Prepend[_1 :: _0 :: HNil, _3 :: _2 :: HNil]
  }
}

trait Sorted[L <: HList] {
  type Out <: HList
}

object Sorted {

  def apply[P <: HList](implicit sorted: Sorted[P]): Aux[P, sorted.Out] = sorted

  type Aux[H <: HList, Out0 <: HList] = Sorted[H] { type Out = Out0 }

  implicit def hnilSorted: Aux[HNil, HNil] = new Sorted[HNil] {
    type Out = HNil
  }

  implicit def hlistSorted[H <: Nat,
                           T <: HList,
                           lsOut <: HList,
                           gsOut <: HList,
                           smOut <: HList,
                           slOut <: HList](
      implicit
      ls: LTEqs.Aux[T, H, lsOut],
      gs: GTs.Aux[T, H, gsOut],
      sortedSmaller: Sorted.Aux[lsOut, smOut],
      sortedLarger: Sorted.Aux[gsOut, slOut],
      preps: Prepend[smOut, H :: slOut]): Aux[H :: T, preps.Out] =
    new Sorted[H :: T] {
      type Out = preps.Out
    }

  def main(args: Array[String]): Unit = {
    type _1 = Succ[_0]
    type _2 = Succ[_1]
    type _3 = Succ[_2]
//    Sorted[_1 :: _0 :: _3 :: _2 :: HNil]
  }
}
