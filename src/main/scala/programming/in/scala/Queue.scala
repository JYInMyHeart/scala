package programming.in.scala

/**
  * @author xck
  */
class Queue[+T] private(
                         private[this] var leading: List[T],
                         private[this] var trailing: List[T]
                       ) {
  private def mirror() =
    if (leading.isEmpty) {
      while (!trailing.isEmpty) {
        leading = trailing.head :: leading
        trailing = trailing.tail
      }
    }

  def head: T = {
    mirror()
    leading.head
  }

  def tail: Queue[T] = {
    mirror()
    new Queue(leading.tail, trailing)
  }

  def enqueue[U >: T](x: U) =
    new Queue[U](leading, x :: trailing)


  override def toString = s"Queue($leading, $trailing)"
}

object Queue {
  def main(args: Array[String]): Unit = {
//    val queue = new Queue[String](List[String]("a", "b"), Nil)
//    val queue1 = queue.enqueue("3")
//    val queue2 = queue1.enqueue("4")
//    val queue3 = queue2.enqueue("5")
//    val c: Any = 2
//    val queue4 = queue3.enqueue(c)
//    println(queue4)
  }
}
