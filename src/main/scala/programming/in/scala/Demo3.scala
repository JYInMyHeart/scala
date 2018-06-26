package programming.in.scala

import scala.language.implicitConversions

class Demo3(val value:Int) {
  def + (demo3: Demo3) = new Demo3(value + demo3.value)
  def + (int: Int) = new Demo3(value + int)

  override def toString: String = s"$value"
}
object Demo3{
  implicit def ->>(demo4: Demo4):Demo3 = new Demo3(demo4.value)
  implicit def ->(str:Double):Demo3 = new Demo3(str.toInt)
  implicit def ->(int: Int):Demo3 = new Demo3(int)

}
class Demo4(val value:Int){
  override def toString: String = s"$value"
}

object Test{
  def main(args: Array[String]): Unit = {
    import Demo3.->>
    val a = new Demo4(4)
    val b = 5
    val c = new Demo3(2)
    val d = 2.0
    println(a + b)
    println(b + c)
    println(d + c)
  }
}


object Mocha extends App{
  class PreferredDrink(val preference:String)

  implicit val pref = new PreferredDrink("mocha")

  def enjoy(name:String)(implicit drink: PreferredDrink) = {
    print(s"welcome $name")
    print(". enjoy a ")
    print(drink.preference)
    print("!")
  }

  enjoy("reader")
}