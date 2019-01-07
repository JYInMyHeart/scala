import java.io.{File, PrintWriter}

import scala.annotation.tailrec

object Test1 extends App {
  def loopThrough(number: Int)(closure: Int => Unit): Unit = {
    for (i <- 1 to number) closure(i)
  }

  var result = 0
  val addIt = { value: Int => result += value }

  loopThrough(0) { elem => addIt(elem) }
  println(result)

  result = 0
  loopThrough(5)(addIt)
  println(result)

}

class Resource private() {
  println("Starting transaction...")

  private def cleanUp(): Unit = {
    println("Ending transaction...")
  }

  def op1(): Unit = println("op1")

  def op2(): Unit = println("op2")

  def op3(): Unit = println("op3")
}

object Resource {
  def use(codeBlock: Resource => Unit): Unit = {
    val resource = new Resource
    try {
      codeBlock(resource)
    } finally {
      resource.cleanUp()
    }
  }

  def writeToFile(fileName: String)(codeBlock: PrintWriter => Unit): Unit = {
    val writer = new PrintWriter(new File(fileName))
    try codeBlock(writer) finally {
      writer.close()
    }
  }


  def main(args: Array[String]): Unit = {
    Resource.use {
      resource =>
        resource.op1()
        resource.op2()
        resource.op3()
        resource.op1()
    }

    //    writeToFile("./output.txt"){
    //    writer => writer write "hello from scala"
    //    }
    trait Friend {
      val name: String

      def listen(): Unit = println(s"Your friend $name is listening!")
    }
    def useFriend(friend: Friend): Unit = friend.listen()

    class Cat(val name: String)
    val c = new Cat("angle") with Friend
    val friend: Friend = c
    friend.listen()
    useFriend(friend)
  }

  @tailrec
  def fac(num:Int,fact:BigInt):BigInt = if (num == 0) fact else fac(num - 1,num * fact)
}