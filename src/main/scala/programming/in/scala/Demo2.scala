package programming.in.scala

import java.awt.event.{ActionEvent, ActionListener}

import javax.swing.JButton
import A.A2B

import scala.language.implicitConversions


object Demo2 {
  def main(args: Array[String]): Unit = {
    val button = new JButton
    button.addActionListener(
      (_: ActionEvent) => println("press")
    )

    val a = new A(2)
    val b = new B(3)

    val c:Int = a + 4
    println(c)


  }
  implicit def function2ActionListener(f:ActionEvent => Unit): ActionListener =
    (e: ActionEvent) => f(e)

}

class B(val value:Int){
  val bb: Int = value
  def +(b:Int) = this.value + b
  
}


class A(val value:Int){

}

object A{
  implicit def A2B(a: A): B = new B(a.value)
}

class MyPair(
              val w:Int,
              val h:Int){


  override def toString: String = s"[$h,$w]"
}

object MyPair{
  implicit class MyPairMaker(width:Int){
    def -= (height:Int):MyPair = new MyPair(width,height)
  }
  def main(args: Array[String]): Unit = {
    val myPair = 3 -= 5
    println(myPair)
  }
}

class MyNums(val value:Int){
  def v:Int = value
  def + (num:MyNums) = new MyNums(this.value + num.value)
  def + (i:Int) = new MyNums(this.value + i)
  def int2MyNum(i:Int):MyNums = new MyNums(i)

}

object MyNums{

}
