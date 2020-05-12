package leetcode

object Testxx {
  def main(args: Array[String]): Unit = {
    val list = for (i <- 1 to 36) yield i
    val startTime = System.currentTimeMillis()
    println("starttime----> " + startTime)
    println(list.toSet.subsets())
    val endTime = System.currentTimeMillis()
    println("endtime----> " + endTime)
    println("duration----> " + (endTime - startTime))
  }

}
