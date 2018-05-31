import scala.collection.mutable.ArrayBuffer

/**
  * @author xck
  */
class Point(val x:Int,val y:Int)
trait Rectangle{
  def topLeft:Point
  def bottomRight:Point
  def left = topLeft.x
  def right = bottomRight.x
  def width = right - left
}

abstract class IntQueue{
  def get():Int
  def put(x:Int)

}
class BasicIntQueue extends IntQueue{
  private val buf = new ArrayBuffer[Int]

  override def get(): Int = buf.remove(0)

  override def put(x:Int) = buf += x
}
trait Doubling extends IntQueue{
  abstract override def put(x: Int): Unit = super.put(2 * x)
}
trait Incrementing extends IntQueue{
  abstract override def put(x: Int): Unit = super.put(x + 1)
}
trait Filtering extends IntQueue{
  abstract override def put(x: Int): Unit = if(x >= 0) super.put(x)
}