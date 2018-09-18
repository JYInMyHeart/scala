package nio.channel

import java.io.IOException
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel

import scala.io.StdIn

class WebClient {
  def run(): Unit = {
    var socketChannel: SocketChannel = null
    try {
      socketChannel = SocketChannel.open()
      socketChannel.connect(new InetSocketAddress("localhost", 8000))
      while (true) {
        val scanner = StdIn.readLine()
        scanner match {
          case "w" =>
            val msg = StdIn.readLine()
            val writeBuffer = ByteBuffer.allocate(128)
            val writeBuffer1 = ByteBuffer.allocate(128)
            writeBuffer.put("hello world".getBytes())
            writeBuffer1.put(msg.getBytes())

            writeBuffer.flip()
            writeBuffer1.flip()
            val bufferArray = Array(writeBuffer, writeBuffer1)
            socketChannel.write(bufferArray)
          case "r" =>
            val readBuffer = ByteBuffer.allocate(128)
            socketChannel.read(readBuffer)
            readBuffer.flip()
            print(socketChannel.getRemoteAddress + ":")
            while (readBuffer.hasRemaining) {
              print(readBuffer.get.toChar)
            }
            println()
          case _ =>
        }
      }


    } catch {
      case e: IOException => e.printStackTrace()
    } finally {
      socketChannel.close()
    }
  }
}

object WebClient extends App {
  new WebClient().run
}
