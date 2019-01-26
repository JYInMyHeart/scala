package programming.in.scala

import scala.util.Try

object Demo5 extends App {
  def maybeTwice(b:Boolean,i: => Int) = {
    lazy val j = i
    if (b) j + j else 0
  }


//  maybeTwice(true,{println("hi");1 + 41})
  1+2

  abstract class Exp
  case class Add(e1:Exp,e2:Exp) extends Exp
  case class Mul(e1:Exp,e2:Exp) extends Exp
  case class Div(e1:Exp,e2:Exp) extends Exp
  case class Min(e1:Exp,e2:Exp) extends Exp


  def parseExp(str:String):Exp = {
    val e1 = parseMul(str)
    parseExp1(str) match {
      case None => e1
      case Some(value) => value(e1)
    }
  }

  def parseMul(str:String):Exp = {
    val e1 = parseNum(str)
    parseMul1(str) match {
      case None => e1
      case Some(v) => v(e1)
    }
  }

  def parseExp1(str:String):Option[Exp => Exp] = {
    null
  }

  def parseMul1(str:String):Option[Exp => Exp] = {
    null
  }

  def parseNum(str:String):Exp = {
    null
  }
}
