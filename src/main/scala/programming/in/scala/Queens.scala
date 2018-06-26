package programming.in.scala

object Queens {

  def inCheck(q1: (Int, Int), q2: (Int, Int)) =
    q1._1 == q2._1 ||
      q1._2 == q2._2 ||
      (q1._1 - q2._1).abs == (q1._2 - q2._2).abs

  def isSafe(queen: (Int, Int), queens: List[(Int, Int)]) =
    queens forall (q => !inCheck(queen, q))

  def queens(n: Int): List[List[(Int, Int)]] = {
    def placeQueens(k: Int): List[List[(Int, Int)]] =
      k match {
        case 0 => List(List())
        case _ => {
          for {
            queens <- placeQueens(k - 1)
            column <- 1 to n
            queen = (k, column)
            if isSafe(queen, queens)
          } yield queen :: queens
        }
      }

    placeQueens(n)
  }

  def main(args: Array[String]): Unit = {
    val list = List(1,2,3,4)
    val list2 = List(1,2,3,4)
    val list1 = List("1","2","3","4")
    (for(x <- list if x % 2 == 0) yield x + 1) foreach print
    println
    list withFilter (_ % 2 == 0) foreach (x => print(x + 1))
    println()
    (for(x <- list if x % 2 == 1;y <- list if y % 2 == 0) yield x + y) foreach print
    println
    list2 withFilter (_ % 2 == 1) flatMap (x => list2 withFilter (_ % 2 == 0) map (y => x + y)) foreach print
    println

    (for(((x,y),z) <- list zip list zip list) yield x + y + z) foreach print
    println

    list zip list zip list map {case ((x,y),z) => x + y + z} foreach print

    println

    for(x <- 1 to 100;y = 23) yield x * y

    println



  }


}
