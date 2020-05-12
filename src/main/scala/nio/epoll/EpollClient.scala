package nio.epoll

import java.io.IOException
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel

class EpollClient {
  def run(msg: String) = {
    try {
      val socketChannel = SocketChannel.open()
      socketChannel.connect(new InetSocketAddress("localhost", 9999 ))

      val writeBuffer = ByteBuffer.allocate(32)
      val readBuffer = ByteBuffer.allocate(32)

      writeBuffer.put(msg.getBytes())
      writeBuffer.flip()



for(i <- 0 until 3) {

  writeBuffer.rewind()
  socketChannel.write(writeBuffer)
  //  Thread.sleep(1000)
  readBuffer.clear()
  socketChannel.read(readBuffer)
  println(prettyInput(readBuffer))

}

       def prettyInput(buffer: ByteBuffer)={
        buffer.flip
        val tbyte =  Array.ofDim[Byte](buffer.limit())
        buffer.get(tbyte)
         new String(tbyte)
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
    new Worker(e1,"hello").run()
    new Worker(e2,"world").run()
    new Worker(e3,"hello3").run()
    new Worker(e4,"hello4").run()
    new Worker(e5,"hello5").run()
  }
}
