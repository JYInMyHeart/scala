trait RNG{
  def nextInt:(Int,RNG)
}
case class SimpleRNG(seed:Long) extends RNG{
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n,nextRNG)
  }

  def randomPair(rng:RNG):(Int,Int) = {
    val (i1,rng2) = rng.nextInt
    val (i2,_) = rng2.nextInt
    (i1,i2)
  }

  def double(rng:RNG):(Double,RNG) = {
    nextInt match {
      case (Int.MaxValue,r) => (0.toDouble,r)
      case (x,r) => ((x / Int.MaxValue).toDouble,r)
    }
  }

  def intDouble(rng:RNG):((Int,Double),RNG) =
    double(rng) match {
      case (x,r) => (((x * Int.MaxValue).toInt,x),r)
    }

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    double(rng) match {
      case (x,r) => ((x,(x * Int.MaxValue).toInt),r)
    }

  def double3(rng:RNG): (Double, Double, Double) =
    (double(rng)._1,double(rng)._1,double(rng)._1)

  def ints(count:Int)(rng:RNG):(List[Int],RNG) = count match {
    case 0 => (Nil,rng)
    case n => (nextInt._1 :: ints(n - 1)(rng)._1,rng)
  }

  def foldr[B](count:Int,b:B)(function: (Int,B) => B):B = count match {
    case 0 => b
    case n => foldr(n - 1,function(n,b))(function)
  }
//  def ints1(count:Int)(rng:RNG):(List[Int],RNG) = (foldr(count,List)(::),rng)
}

object RNG{
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(5)
    println(rng.randomPair(rng))

  }
}

