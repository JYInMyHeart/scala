
class A{
  override def toString = s"A()"
}
object Test extends App{
  val t = if(1 > 2) 3 else new A
//  println(t + 1)
  val s:Any = 1
  println(s.toString + t)

}
