package freemonad

object Demo1 {
  def f[T](c: Id ~> Id, head: String, tail: String): Id[_] = {
    if (c(true, false)) {
      c(head, tail)
    } else
      c(tail, head)
  }

  trait ~>[F[_], G[_]] {
    def apply[A](a: F[A], b: F[A]): G[A]
  }

  type Id[A] = A

  def main(args: Array[String]): Unit = {
    val cs = new (Id ~> Id) {
      override def apply[A](a: Id[A], b: Id[A]): Id[A] =
        if (math.random() < 0.5) a else b

    }
    println(f(cs, "a", "b"))
  }

}
