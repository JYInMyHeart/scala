package c89
import org.scalatest.FunSuite

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks

class LexerTest {

}

object LexerTest extends App{
//  var a = 0
//  for(i <- 1 to 10){
//
//    val loop = new Breaks
//    loop.breakable{
//      a =a + 1
//      if(i % 3 == 0){
//        loop.break()
//      }else
//        println(i)
//    }
//  }
//  println(a)
  val l = ListBuffer[Int](0)
  val m = mutable.HashMap[String,ListBuffer[Int]]("a" -> l)
  println(m)
  l += 4



  println(l)
  println(m)
}
