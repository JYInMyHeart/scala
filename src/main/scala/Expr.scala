/**
  * @author xck
  */
sealed abstract class Expr

case class Var(name: String) extends Expr

case class Number(num: Double) extends Expr

case class UnOp(operator: String, arg: Expr) extends Expr

case class BinOp(operator: String, left: Expr, right: Expr) extends Expr


object Expr {

  def describe(e: Expr): String = e match {
    case Number(_) => "a number"
    case Var(_) => "a variable"
  }

  def simplifyTop(expr: Expr): Expr = expr match {
    case UnOp("-", UnOp("-", e)) => e
    case BinOp("+", e, Number(0)) => e
    case BinOp("*", e, Number(1)) => e
    case _ => expr
  }

  def main(args: Array[String]): Unit = {
    println(simplifyTop(UnOp("-", UnOp("-", Var("x")))))
    val d = 3
    d match {
      case x => if (x > 3) println(x)
      case x => if (x == 2) println(2 * x)
      case _ => println("noob")
    }
  }

  def checkLess[A, B](pf: PartialFunction[A, B]): A => B = pf: A => B


}