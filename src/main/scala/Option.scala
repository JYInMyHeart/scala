
import java.util.regex.{Pattern, PatternSyntaxException}

import MyList.List
import MyList.Cons
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => this
    case _ => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

  def variance(xs: Seq[Double]): Option[Double] = flatMap((x) => Some(math.pow(xs.sum / xs.size, 2)))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  val abs0: Option[Double] => Option[Double] = lift(math.abs)

  def insuraceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
    age + numberOfSpeedingTickets
  }

  def parseInsuraceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    //Some(insuraceRateQuote(opTop(optAge), opTop(optTickets)))
    map2(optAge, optTickets)(insuraceRateQuote)
  }

  def opTop[A](option: => Option[A]): A = option match {
    case Some(x) => x
  }

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {
      case e: Exception => None
    }
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(x), Some(y)) => Some(f(x, y))
    case _ => None
  }

  def map21[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap (aa => b map (bb => f(aa, bb)))

  def map22[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    aa <- a
    bb <- b
  } yield f(aa, bb)

  def pattern(str: String):Option[Pattern] =
    try{
      Some(Pattern.compile(str))
    }catch {
      case e:PatternSyntaxException => None
    }

  def mkMather(pat:String):Option[String => Boolean] =
    pattern(pat) map (p => (s:String) => p.matcher(s).matches)



  def getOne[A](a: List[Option[A]]): Option[A] = a match {
    case Cons(Some(x), y) => Some(x)
    case _ => None
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    Try(List.map(List.filter(a)(_ != None))(opTop(_)))

  //def parseInts(a: List[String]): Option[List[Int]] = sequence(List.map(a)(Try(_)))

  def main(args: Array[String]): Unit = {
    println(traverse(List("1", "2", "3"))(Try(_)))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    Try(List.map(List.filter(a)(_ != None))(i => opTop(f(i))))



}


case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]
