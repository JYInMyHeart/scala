object Main1 {
  def main(args: Array[String]): Unit = {
    val l1 = List(1,2,3)
    val l2 = List("a","b","c")
    println(l1.zip(l2).toMap)

  }

}
