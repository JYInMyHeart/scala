
import java.util.regex.{Pattern, PatternSyntaxException}

import MyList.List
import MyList.Cons
sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(a) => MySome(f(a))
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(a) => f(a)
  }


  def getOrElse[B >: A](default: => B): B = this match {
    case MySome(a) => a
    case MyNone => default
  }

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this match {
    case MyNone => this
    case _ => ob
  }

  def filter(f: A => Boolean): MyOption[A] = this match {
    case MySome(a) if f(a) => this
    case _ => MyNone
  }

  def variance(xs: Seq[Double]): MyOption[Double] = flatMap((x) => MySome(math.pow(xs.sum / xs.size, 2)))

  def lift[A, B](f: A => B): MyOption[A] => MyOption[B] = _ map f

  val abs0: MyOption[Double] => MyOption[Double] = lift(math.abs)

  def insuraceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
    age + numberOfSpeedingTickets
  }

  def parseInsuraceRateQuote(age: String, numberOfSpeedingTickets: String): MyOption[Double] = {
    val optAge: MyOption[Int] = Try(age.toInt)
    val optTickets: MyOption[Int] = Try(numberOfSpeedingTickets.toInt)
    //Some(insuraceRateQuote(opTop(optAge), opTop(optTickets)))
    map2(optAge, optTickets)(insuraceRateQuote)
  }

  def opTop[A](option: => MyOption[A]): A = option match {
    case MySome(x) => x
  }

  def Try[A](a: => A): MyOption[A] = {
    try MySome(a)
    catch {
      case e: Exception => MyNone
    }
  }

  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = (a, b) match {
    case (MySome(x), MySome(y)) => MySome(f(x, y))
    case _ => MyNone
  }

  def map21[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = a flatMap (aa => b map (bb => f(aa, bb)))

  def map22[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = for {
    aa <- a
    bb <- b
  } yield f(aa, bb)

  def pattern(str: String):MyOption[Pattern] =
    try{
      MySome(Pattern.compile(str))
    }catch {
      case e:PatternSyntaxException => MyNone
    }

  def mkMather(pat:String):MyOption[String => Boolean] =
    pattern(pat) map (p => (s:String) => p.matcher(s).matches)



  def getOne[A](a: List[MyOption[A]]): MyOption[A] = a match {
    case Cons(MySome(x), y) => MySome(x)
    case _ => MyNone
  }

  def sequence[A](a: List[MyOption[A]]): MyOption[List[A]] =
    Try(List.map(List.filter(a)(_ != MyNone))(opTop(_)))

  //def parseInts(a: List[String]): Option[List[Int]] = sequence(List.map(a)(Try(_)))

  def main(args: Array[String]): Unit = {
    println(traverse(List("1", "2", "3"))(Try(_)))
  }

  def traverse[A, B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] =
    Try(List.map(List.filter(a)(_ != MyNone))(i => opTop(f(i))))



}

object MyOption{
  def empty[A]:MyOption[A] = MyNone
}


case class MySome[+A](get: A) extends MyOption[A]

case object MyNone extends MyOption[Nothing]
