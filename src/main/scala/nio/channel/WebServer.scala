package nio.channel

import java.io.IOException
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import java.util.concurrent.Executors


import scala.io.StdIn

object WebServer extends App {
  var ssc: ServerSocketChannel = null
  var socketChannel: SocketChannel = null
  val execute = Executors.newFixedThreadPool(4)
  try {
    ssc = ServerSocketChannel.open()

    ssc.socket().bind(new InetSocketAddress(8000))
    while (true) {
      try {
        socketChannel = ssc.accept()

        val task = new Worker(socketChannel)
        execute.submit(task)
      } catch {
        case e: IOException => e.printStackTrace()
      }
    }
  }
  catch {
    case e: IOException => e.printStackTrace()
  }
  finally {
    socketChannel.close()
    ssc.close()
    execute.shutdown()
  }


}

class Worker extends Runnable {
  var sc: SocketChannel = _

  def this(ssc: SocketChannel) = {
    this
    this.sc = ssc
  }

  override def run(): Unit = {
    while (true) {
      val scanner = StdIn.readLine()

      scanner match {
        case "w" =>
          val msg = StdIn.readLine()
          val writeBuffer = ByteBuffer.allocate(128)
          writeBuffer.put(msg.getBytes())
          writeBuffer.flip()
          sc.write(writeBuffer)
          println("Server:" + msg)
        case "r" =>
          val readBuffer = ByteBuffer.allocate(8)
          val readBuffer1 = ByteBuffer.allocate(8)
          sc.read(readBuffer)
          sc.read(readBuffer1)

          readBuffer.flip()
          readBuffer1.flip()
          print(sc.getRemoteAddress + ":")
          while (readBuffer.hasRemaining) {
            print(readBuffer.get.toChar)
          }
          while (readBuffer1.hasRemaining) {
            print(readBuffer1.get.toChar)
          }
          println()
        case _ =>
      }
    }

  }
}