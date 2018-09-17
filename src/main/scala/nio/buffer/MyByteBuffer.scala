package nio.buffer

import scala.StringContext.InvalidEscapeException

class MyByteBuffer {
  var capacity:Int = _
  var limit:Int = _
  var position:Int = _
  var mark:Int = _
  var bn:Array[Byte] = _

  def put(b:Byte):Unit = {
    checkSize()
    bn(position) = b
    position += 1
    checkSize()
  }

  def get():Byte = {
    checkSize()
    val b = bn(position)
    position += 1
    checkSize()
    b
  }

  def flip = {
    limit = position
    position = 0
    mark = -1
  }

  def clear = {
    limit = capacity
    position = 0
    mark = -1
  }

  def remaining:Int = {
    limit - position
  }

  def rewind = {
    position = 0
    mark = -1
  }

  def marked = {
    mark = position
  }

  def reset = {
    if(mark < 0) throw new InvalidEscapeException(1)
    position = mark
  }

  def checkSize() = {
    position match {
      case x if x > limit || x < 0 =>
        throw new IndexOutOfBoundsException
      case _  =>
    }
  }
}
object MyByteBuffer{
  def allocate(size:Int) = {
    val myByteBuffer = new MyByteBuffer()
    myByteBuffer.bn = Array.ofDim[Byte](size)
    myByteBuffer.capacity = size
    myByteBuffer.limit = size
    myByteBuffer.position = 0
    myByteBuffer.mark = -1
    myByteBuffer
  }
}
