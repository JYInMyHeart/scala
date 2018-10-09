trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {


  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def randomPair(rng: RNG): (Int, Int) = {
    val (i1, rng2) = rng.nextInt
    val (i2, _) = rng2.nextInt
    (i1, i2)
  }

  def double(rng: RNG): (Double, RNG) = {
    nextInt match {
      case (Int.MaxValue, r) => (0.toDouble, r)
      case (x, r) => (x.toDouble / Int.MaxValue, r)
    }
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    double(rng) match {
      case (x, r) => ((x, (x * Int.MaxValue).toInt), r)
    }

  def double3(rng: RNG): (Double, Double, Double) =
    (double(rng)._1, double(rng)._1, double(rng)._1)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (Nil, rng)
    case n => (int(rng)._1 :: ints(n - 1)(int(rng)._2)._1, ints(n - 1)(int(rng)._2)._2)
  }


  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    var (a, b) = rng.nextInt
    a match {
      case x if x < 0 =>
        a += Int.MaxValue + 1
      case x => x
    }
    (a, b)
  }


  def double1(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(a => a.toDouble / Int.MaxValue)(rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }


  def intDouble1(rng: RNG): ((Int, Double), RNG) =
    map2(int, double)((_, _))(rng)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill[Rand[Int]](count)(int))(rng)

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rnga) = f(rng)
      g(a)(rnga)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    rng =>
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        (mod, rng2)
      else
        nonNegativeLessThan(n)(rng)
  }

  def nonNegativeLessThan1(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }


  def map1[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map21[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map1(rb) (b => f(a,b)))


}

object RNG {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(6)
    println(rng.double(rng))
    println(rng.double1(rng))
    println(rng.doubleInt(rng))
    println(rng.intDouble(rng))
    println(rng.intDouble1(rng))
    println(rng.randIntDouble(rng))
    println(rng.ints(5)(rng))
    println(rng.ints2(5)(rng))
    println(rng.nonNegativeLessThan(900)(rng))
    println(rng.nonNegativeLessThan1(900)(rng))
    println(rng.map2(rng.int, rng.double)((_, _))(rng))
    println(rng.map21(rng.int, rng.double)((_, _))(rng))

  }
}

