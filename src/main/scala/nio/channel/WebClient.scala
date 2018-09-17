package nio.channel

import java.io.IOException
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel

object WebClient extends App{
  var socketChannel:SocketChannel = null
  try{
    socketChannel = SocketChannel.open()
    socketChannel.connect(new InetSocketAddress("localhost",8000))

    while(true){
      val writeBuffer = ByteBuffer.allocate(128)
      val writeBuffer1 = ByteBuffer.allocate(128)
      writeBuffer.put("hello world".getBytes())
      writeBuffer1.put("i am boy".getBytes())

      writeBuffer.flip()
      writeBuffer1.flip()
      val  bufferArray = Array(writeBuffer,writeBuffer1)
      socketChannel.write(bufferArray)


      val readBuffer = ByteBuffer.allocate(128)
      socketChannel.read(readBuffer)
      readBuffer.flip()
      print(socketChannel.getRemoteAddress + ":")
      while(readBuffer.hasRemaining){
        print(readBuffer.get.toChar)
      }
      println()
    }


  }catch {
    case e:IOException => e.printStackTrace()
  }finally {
    socketChannel.close()
  }

}
