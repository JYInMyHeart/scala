/**
  * @author xck
  */
class Dollars(val amount:Int) extends AnyVal {


  override def toString = "$" + s"$amount"
}
class SwissFrancs(val amount:Int) extends AnyVal{
  override def toString = s"$amount CHF"
}
object Dollars{
  def main(args: Array[String]): Unit = {
    val frog = new Frog
    println(frog)
  }
}

trait Philosophical{
  def philosophize() = {
    println("I consume memory, therefore I am!")
  }
}

class Frog extends Philosophical{

  override def toString = "green"
}