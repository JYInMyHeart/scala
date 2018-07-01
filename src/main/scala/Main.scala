object Main {
  def main(args: Array[String]): Unit = {
    def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
      if (cond) onTrue() else onFalse()

    if2(21 < 22, () => println("a"), () => println("b"))

    def twice(b: Boolean, i: => Int) = if (b) i + i else 0

    val x = twice(true, {
      println("sb");
      41 + 1
    })
    println(x)

    def sum(xs:List[Int]):Int = xs.foldRight(0)(_+_)

    println(sum(List(1, 2, 3)))

  }
}
