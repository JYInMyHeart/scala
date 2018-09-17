package programming.in.scala

/**
  * @author xck
  */
object Demo1 {
  def main(args: Array[String]): Unit = {


    val l1 = Array[Int](1,2,3)
    val l2 = Array[Int](4,5,6)
    val l3 = Array[Int](7,8,9)
    val l = Array(l1,l2,l3)


    transpose(l).foreach(_.map(print))



  }

  def transpose(A: Array[Array[Int]]): Array[Array[Int]] = {
    val m = A.length
    val n = if(m > 0) A(0).length else 0
    val temp:Array[Array[Int]] = Array.ofDim(n,m)
    for(i <- A.indices;v <- A)
        for (i1 <- v.indices)
          temp(i1)(i) = A(i)(i1)

    temp
  }
}
