package freemonad

class Demo {
  type ¬[A] = A => Nothing
  type ∨[T, U] = ¬[¬[T] with ¬[U]]
  type ¬¬[A] = ¬[¬[A]]
  type |∨|[T, U] = {
    type λ[X] = ¬¬[X]

  }
  def size[T: (Int |∨| String)#λ](t: T) = t match {
    case i: Int => i
    case s: String => s.length
  }

  def main(args: Array[String]): Unit = {
   type Expr = Fix[ExprF]



  }
  def const(i: Int) = Fix[({type λ[α] = ExprF[Nothing]})#λ](Const(i)).asInstanceOf[Fix[ExprF]]
  def mul[A](a: A, b: A) = Fix[({type λ[α] = ExprF[A]})#λ](Mul(a, b)).asInstanceOf[Fix[ExprF]]
  def add[A](a: A, b: A) = Fix[({type λ[α] = ExprF[A]})#λ](Add(a, b)).asInstanceOf[Fix[ExprF]]

  val testExpr1 = add(const(2), mul(const(3), const(4)))



}
trait ExprF[+A]
case class Const[A](i: Int) extends ExprF[A]
case class Add[A](left: A, right: A) extends ExprF[A]
case class Mul[A](left: A, right: A) extends ExprF[A]

case class Fix[F[_]](f:F[Fix[F]])