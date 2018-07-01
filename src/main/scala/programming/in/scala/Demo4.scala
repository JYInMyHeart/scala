package programming.in.scala

object Demo14 {
  def insert(i: Int, ints: List[Int]): List[List[Int]] = (i, ints) match {
    case (_, Nil) => Nil
    case (n, x :: xs) =>
      List(n :: x :: xs) ::: insert(n, xs).map(b => x :: b)
  }

  def main(args: Array[String]): Unit = {
    println(insert(2, List(1, 3, 4)))
  }


}

class Exp

case class Val(value: Double) extends Exp

case class Plus(val1: Exp, val2: Exp) extends Exp

case class Sub(val1: Exp, val2: Exp) extends Exp

case class Mult(val1: Exp, val2: Exp) extends Exp

case class Div(val1: Exp, val2: Exp) extends Exp

object Exp{
  def eval(exp:Exp):Double = exp match {
    case Val(a) => a
    case Plus(v1,v2) => eval(v1) + eval(v2)
    case Sub(v1,v2) => eval(v1) - eval(v2)
    case Mult(v1,v2) => eval(v1) * eval(v2)
    case Div(v1,v2) => eval(v1) / eval(v2)
  }

  def showExp(exp:Exp):String = exp match {
    case Val(a) => a.toString
    case Plus(v1,v2) => s"(${showExp(v1)} + ${showExp(v2)})"
    case Sub(v1,v2) => s"(${showExp(v1)} - ${showExp(v2)})"
    case Mult(v1,v2) => s"(${showExp(v1)} * ${showExp(v2)})"
    case Div(v1,v2) => s"(${showExp(v1)} / ${showExp(v2)})"
  }

  def divide[A](list:List[A]):List[(List[A],List[A])] = {
    (for(n <- 1 until list.size) yield (list.take(n),list.drop(n))).toList
  }

  def buildExpressions(exp:(List[Exp],List[Exp])):List[Exp] = exp match {
    case (es1,es2) => {
      for(op <- List(Plus,Sub,Mult,Div);e1 <- es1;e2 <- es2)
        yield op(e1,e2)
    }
  }

  def toExpression(ds:List[Double]):List[Exp] = ds match {
    case Nil => Nil
    case x::Nil => List(Val(x))
    case xs =>
      (for ((l, r) <- divide(xs))
        yield buildExpressions(toExpression(l), toExpression(r)))
        .reduce(_ ++ _)
  }

  def generate(ds:List[Double]):List[Exp] =
    ds.permutations.map(toExpression).reduce(_ ++ _)

  def twentyfour(ds:List[Double]):List[String] =
    generate(ds).filter(x => eval(x) == 24.0).map(showExp)

  def main(args: Array[String]): Unit = {
    println(twentyfour(List(4.0, 2.0, 3.0)))
  }

}

