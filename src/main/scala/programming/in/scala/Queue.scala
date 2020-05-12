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

//class MyQueue[+T]{
//  def append(x:T) = {
//
//  }
//}

//class StrangeIntQueue extends Queue[Int]{
//  override def append(x:Int) = {
//    super.append(x)
//  }
//}

object Queue {

  def orderedMergeSort[T <: Ordered[T]](xs: List[T]): List[T] = {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (x < y) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (ys, zs) = xs splitAt n
      merge(orderedMergeSort(ys), orderedMergeSort(zs))
    }
  }

  class Person(val firstName: String, val lastName: String) extends Ordered[Person]{
    override def compare(that: Person): Int = {
      val lastNameComparison =
        lastName.compareToIgnoreCase(that.lastName)
      if (lastNameComparison != 0)
        lastNameComparison
      else
        firstName.compareToIgnoreCase(that.firstName)
    }

    override def toString = firstName +" "+ lastName
  }

  def main(args: Array[String]): Unit = {
    val queue = new Queue[String](List[String]("a", "b"), Nil)
    val queue1 = queue.enqueue("3")
    val queue2 = queue1.enqueue("4")
    val queue3 = queue2.enqueue("5")
    val c: Any = 2
    val queue4 = queue3.enqueue(c)
    println(queue4)

    println(orderedMergeSort(List(new Person("Larry", "Wall"),
      new Person("Anders", "Hejlsberg"),
      new Person("Guido", "van Rossum"),
      new Person("Alan", "Kay"),
      new Person("Yukihiro", "Matsumoto"))))


  }
}
