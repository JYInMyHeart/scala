package nio.epoll

import java.io.IOException
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel

class EpollClient {
  def run(msg: String) = {
    try {
      val socketChannel = SocketChannel.open()
      socketChannel.connect(new InetSocketAddress("localhost", 8000))

      val writeBuffer = ByteBuffer.allocate(32)
      val readBuffer = ByteBuffer.allocate(32)

      writeBuffer.put(msg.getBytes())
      writeBuffer.flip()

      while (true) {
        writeBuffer.rewind()
        socketChannel.write(writeBuffer)
        readBuffer.clear()
        socketChannel.read(readBuffer)
      }
    } catch {
      case e: IOException =>
        e.printStackTrace()
    }
  }
}

class Worker(e:EpollClient,msg:String) extends Thread{
  override def run(): Unit = {
    e.run(msg)
  }
}
object EpollClient{
  def main(args: Array[String]): Unit = {
    val e1 = new EpollClient
    val e2 = new EpollClient
    val e3 = new EpollClient
    val e4 = new EpollClient
    val e5 = new EpollClient
    new Worker(e1,"hello1").start()
    new Worker(e2,"hello2").start()
    new Worker(e3,"hello3").start()
    new Worker(e4,"hello4").start()
    new Worker(e5,"hello5").start()
  }
}
