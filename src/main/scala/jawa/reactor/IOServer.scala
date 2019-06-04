package jawa.reactor

import java.net.{ServerSocket, Socket}
import java.time.Clock



class IOServer {
  val serverSocket = new ServerSocket(8000)
  new Thread(() => {
    while (true) {
      try {
        val socket = serverSocket.accept()
        new Thread(() => {
          try {
            var len = 0
            val data = Array.ofDim[Byte](1024)
            val inputStream = socket.getInputStream
            while (len != -1) {
              len = inputStream.read(data)
              println(new String(data, 0, len))
            }

          } catch {
            case _: Exception =>
          }
        }).start()
      } catch {
        case _: Exception =>
      }
    }
  }).start()
}

class IOClient {
  new Thread(() => {
    try {
      val socket = new Socket("127.0.0.1", 8000)
      while (true) {
        try {
          socket.getOutputStream.write(
            (Clock.systemUTC().instant() + ": hello world").getBytes())
          Thread.sleep(1000)
        } catch {
          case _ =>
        }
      }
    } catch {
      case _ =>
    }
  }).start()
}

object Test extends App {
  val server = new IOServer
  val client1 = new IOClient
  val client2 = new IOClient
}
