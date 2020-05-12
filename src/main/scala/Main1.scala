import scala.collection.mutable
object Main1 {
  def main(args: Array[String]): Unit = {
    fun(Array(1, 2, 3, 4, 5, 6), 7).map(println)
  }

  def fun(candiadates: Array[Int], target: Int) = {
    val c = candiadates.sorted
    val table =
      Array.fill[mutable.HashSet[List[Int]]](target + 1)(
        mutable.HashSet[List[Int]]()
      )
    for (i <- c) {
      if (i <= target) {
        for (j <- target - i to 0 by -1) {
          for (k <- table(j)) {
            table(i + j) = table(i + j).union(mutable.HashSet(k ++ List(i)))
          }
        }
      }
      table(i) += List(i)
    }
    table(target)
  }

}
