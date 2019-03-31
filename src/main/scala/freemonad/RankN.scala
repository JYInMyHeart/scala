package freemonad

object RankN extends App {
  def applyToTuple[A](f: List[A] => Int,t:(List[A],List[A])) = (f(t._1),f(t._2))

  def f[A](a:List[A]):Int = a.length

  type Id[A] = A

  trait ~>[F[_],G[_]]{
    def apply[A](fa:F[A]):G[A]
  }
  val singletonList = new (Id ~> List) {
    def apply[A](a: A): List[A] = List(a)
  }

  def apply[A,B](f: Id ~> List, b: B, s: String): (List[B], List[String]) =
    (f(b), f(s))

  println(applyToTuple(f, (List(1, 2, 3), List("1", "2"))))
  println(apply(singletonList,2,"2"))


}
