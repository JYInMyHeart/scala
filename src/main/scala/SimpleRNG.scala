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
}

object RNG{
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(5)
    println(rng.randomPair(rng))

  }
}
