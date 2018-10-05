package nio.epoll

import java.io.IOException
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}

object EpollServer extends App{
  try{
    val ssc = ServerSocketChannel.open()
    ssc.socket().bind(new InetSocketAddress("localhost",8000))
    ssc.configureBlocking(false)

    val selector = Selector.open()
    ssc.register(selector,SelectionKey.OP_ACCEPT)

    val readBuffer = ByteBuffer.allocate(1024)
    val writeBuffer = ByteBuffer.allocate(128)
    writeBuffer.put("received".getBytes())
    writeBuffer.flip()

    while(true){
      val nReady = selector.select()
      val keys = selector.selectedKeys()
      val it = keys.iterator()

      while(it.hasNext){
        val key = it.next()
        it.remove()

        key match {
          case x if x.isAcceptable =>
            val socketChannel = ssc.accept()
            socketChannel.configureBlocking(false)
            socketChannel.register(selector,SelectionKey.OP_READ)
          case x if x.isReadable =>
            val socketChannel = key.channel().asInstanceOf[SocketChannel]
            readBuffer.clear()
            socketChannel.read(readBuffer)

            readBuffer.flip()
            println("received :" + readBuffer.array().filter(_ != 0).map(_.toChar).:\("")(_+_))
            key.interestOps(SelectionKey.OP_WRITE)
          case x if x.isWritable =>
            writeBuffer.rewind()
            val socketChannel = key.channel().asInstanceOf[SocketChannel]
            socketChannel.write(writeBuffer)
            key.interestOps(SelectionKey.OP_READ)
        }
      }
    }
  }catch{
    case e:IOException =>
      e.printStackTrace()
  }
}
