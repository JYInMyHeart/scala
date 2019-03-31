package programming.in.scala

class MyQueue {

}

class SlowAppendQueue[T](elems: List[T]) {
  def head: T = elems.head

  def tail: List[T] = elems.tail

  def enqueue(x: T) = new SlowAppendQueue[T](elems ::: List(x))
}

class FastQueue[T](
                    private val leading: List[T],
                    private val trailing: List[T]
                  ) {
  private def mirror =
    if (leading.isEmpty)
      new FastQueue[T](trailing.reverse, Nil)
    else
      this

  def head: T = mirror.leading.head

  def tail: FastQueue[T] = {
    val q = mirror
    new FastQueue[T](q.leading.tail, q.trailing)
  }

  def enqueue(x: T) =
    new FastQueue[T](leading, x :: trailing)
}

object QueueMain extends App{
  val q = new FastQueue[Integer](List(1),List(2))
  println(q)
}

trait Queue1[T]{
  def head:T
  def tail:Queue[T]
  def enqueue(x:T):Queue[T]
}

object Queue1{
//  def apply[T](xs:T*): Queue1[T] = new Queue1Impl[T](xs.toList,Nil)

  private class Queue1Impl[T](
                               private val leading: List[T],
                               private val trailing: List[T]
                             ) {
    private def mirror =
      if (leading.isEmpty)
        new Queue1Impl[T](trailing.reverse, Nil)
      else
        this

    def head: T = mirror.leading.head

    def tail: FastQueue[T] = {
      val q = mirror
      new FastQueue[T](q.leading.tail, q.trailing)
    }

    def enqueue(x: T) =
      new FastQueue[T](leading, x :: trailing)
  }
}


