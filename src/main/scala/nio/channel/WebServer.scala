package nio.channel

import java.io.IOException
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{ServerSocketChannel, SocketChannel}

import scala.io.StdIn

object WebServer extends App{
  var ssc:ServerSocketChannel = null
  var socketChannel:SocketChannel = null
  try{
    ssc = ServerSocketChannel.open()
    ssc.socket().bind(new InetSocketAddress(8000))
    while(true){
      socketChannel = ssc.accept()

      val readBuffer = ByteBuffer.allocate(8)
      val readBuffer1 = ByteBuffer.allocate(8)
      socketChannel.read(readBuffer)
      socketChannel.read(readBuffer1)

      readBuffer.flip()
      readBuffer1.flip()
      print(socketChannel.getRemoteAddress + ":")
      while(readBuffer.hasRemaining){
        print(readBuffer.get.toChar)
      }
      while(readBuffer1.hasRemaining){
        print(readBuffer1.get.toChar)
      }
      println()

      val scanner = StdIn.readLine()
      val writeBuffer = ByteBuffer.allocate(128)
      writeBuffer.put(scanner.getBytes())
      writeBuffer.flip()
      socketChannel.write(writeBuffer)
      println("Server:" + scanner)
    }
  }catch {
    case e:IOException => e.printStackTrace()
  }finally {
    socketChannel.close()
    ssc.close()
  }
}
