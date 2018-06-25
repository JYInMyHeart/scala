package programming.in.scala

class MyPair(val height: Int,
             val width: Int,
             val length:Int) {

  override def toString: String = s"[$height,$width,$length]"
}

object MyPair{
  implicit class PairHelper(l:Int){
    def build(h:Int)(implicit w:Int) = new MyPair(h,w,l)
  }
  implicit class PairStringHelper(ls:String){
    def build(h:String)(implicit w:String) = new MyPair(h.toInt,w.toInt,ls.toInt)
  }
  implicit val l:Int = 10
  implicit val ls:String = "10"
  def main(args: Array[String]): Unit = {
    val a = 3 build 4
    val b = "4" build "5"
    println(a)
    println(b)
    println(maxList(List(1, 3, 2, 4)))
  }

  def maxList[T :Ordering[T]](element: List[T]):T =
    element match {
      case List() => throw new IllegalArgumentException("empty list")
      case List(x) => x
      case x :: rest =>
        val maxRest = maxList(rest)
        if(implicitly[Ordering[T]].gt(x,maxRest)) x
        else maxRest
    }
}
