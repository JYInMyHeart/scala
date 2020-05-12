package hs

class DT2 {}

import shapeless._
import shapeless.ops.nat.{Diff, LTEq}
sealed trait Tree[+T] {
  type N <: Nat
}
object Leaf extends Tree[Nothing] {
  type N = _0
}
trait Branch[T] extends Tree[T] {
  def value: T
  def left: Tree[T]
  def right: Tree[T]
}
//object Branch {
//  type Aux[T, N0 <: Nat] = Branch[T] { type N = N0 }
//  def apply[T, D <: Nat](value0: T, left0: Tree[T], right0: Tree[T])(
//    implicit diff: Diff.Aux[left0.N, right0.N, D],
//    lte: LTEq[D, _1]
//  ): Aux[T, Succ[left0.N]] =
//    new Branch[T] {
//      val value = value0
//      val left = left0
//      val right = right0
//      type N = Succ[left0.N]
//    }
//}
