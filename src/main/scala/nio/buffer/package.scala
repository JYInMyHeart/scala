package nio.buffer

object Test extends App{
  val buffer = MyByteBuffer.allocate(8)
  buffer.put(0x01)
  buffer.put(0x02)
  buffer.put(0x03)
  buffer.put(0x04)
  buffer.bn.foreach(print)
  println()
  buffer.flip
  println(buffer.get())
  println(buffer.get())
  println(buffer.get())
  println(buffer.get())
}
