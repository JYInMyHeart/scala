package hs

trait Foldable[F[_]]{
  def foldl[A,B](as:F[A],z:B,f:(B,A) => B): B
  def foldMap[A,B](as:F[A],f:A => B)(implicit m:Monoid[B]):B =
    foldl(as,m.zero,(b:B,a:A) => m.add(b,f(a)))
}

object FF extends App {
  def sumOf[F[_]](ns:F[Int])(implicit ff:Foldable[F]) =
    ff.foldl(ns,0,(x:Int,y:Int) => x + y)

  implicit val listFoldable: Foldable[List] = new Foldable[List] {
    override def foldl[A, B](as: List[A], z: B, f: (B, A) => B): B = as.foldLeft(z)(f)
  }

  val sumOfOneTwo = sumOf(List(1,2,3))
  println(sumOfOneTwo)

  implicit val sumMonoid: Monoid[Sum] = new Monoid[Sum]{
    def zero = Sum(0)
    def add(a:Sum,b:Sum) = Sum(a.value + b.value)
  }

  implicit val productMonoid = new Monoid[Product] {
    def zero = Product(1)
    def add(a:Product,b:Product) = Product(a.value * b.value)
  }

  def mapReduce[F[_],A,B](as:F[A],f:A => B)
                         (implicit ff :Foldable[F],m:Monoid[B]) =
    ff.foldMap(as,f)

  val sumOf123 = mapReduce(List(1,2,3),Sum)
  val product123 = mapReduce(List(4,5,6),Product)
  println(sumOf123.value)
  println(product123)
}

trait Monoid[M]{
  def zero:M
  def add(m1:M,m2:M):M
}

case class Sum(value:Int)
case class Product(value:Int)




