package leetcode

import java.util

import scala.language.postfixOps

object Leetcode {
  def main(args: Array[String]): Unit = {
    val a = Array(1,0,1,1)
    val a1 = Array(1,0,0,1)
    val b = Array(a,a1)
    flipAndInvertImage(b).foreach(x => println(util.Arrays.toString(x)))
    println(util.Arrays.toString(reserve1(Array(1, 2, 3, 4))))

  }

  def flipAndInvertImage(A: Array[Array[Int]]): Array[Array[Int]] = {
    for(a <- A)  yield a.reverse.map(x => if (x == 0) x + 1 else x - 1)
  }

  def isBilateralSymmetry(A:Array[Int]):Boolean = {
    A.toList match {
      case x1 +: x2 :+ x3 => if(x1 == x3) isBilateralSymmetry(x2.toArray) else false
      case _ => true
    }
  }

  def reserve1(A:Array[Int]):Array[Int] = A.toList match {
    case x::xs => reserve1(xs.toArray).toList :+ x toArray
    case Nil => Nil.toArray
  }

  def arrayNesting(nums: Array[Int]): Int = {
    0
  }

}
