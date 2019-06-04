package jawa.reactor

import java.nio.charset.Charset
import java.time.Clock
import java.util.concurrent.TimeUnit

import io.netty.bootstrap.Bootstrap
import io.netty.buffer.ByteBuf
import io.netty.channel.{
  ChannelHandlerContext,
  ChannelInboundHandlerAdapter,
  ChannelInitializer
}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.util.concurrent.Future

class NettyClient {
  val workerGroup = new NioEventLoopGroup()
  val bootstrap = new Bootstrap()
  bootstrap
    .group(workerGroup)
    .channel(classOf[NioSocketChannel])
    .handler(new ChannelInitializer[SocketChannel] {
      override def initChannel(ch: SocketChannel): Unit = {
        ch.pipeline().addLast(new FirstClientHandler())
      }
    })
}

class FirstClientHandler extends ChannelInboundHandlerAdapter {
  override def channelActive(ctx: ChannelHandlerContext): Unit = {
    println(s"${Clock.systemUTC().instant()}:client write data ")
    val buffer = getByteBuf(ctx)
    ctx.channel().writeAndFlush(buffer)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: Any): Unit = {
    val byteBuf = msg.asInstanceOf[ByteBuf]
    println(
      s"${Clock.systemUTC().instant()}:client read data -> ${byteBuf.toString(
        Charset.forName("utf-8"))}")
  }

  def getByteBuf(context: ChannelHandlerContext): ByteBuf = {
    val buffer = context.alloc().buffer()
    val bytes = "hello flash!".getBytes(Charset.forName("utf-8"))
    buffer.writeBytes(bytes)
    buffer
  }
}

object NettyClient extends App {
  def connect(bootstrap: Bootstrap,
              host: String,
              port: Int,
              retry: Int): Unit = {
    bootstrap
      .connect(host, port)
      .addListener((future: Future[_ >: Void]) =>
        future.isSuccess match {
          case true =>
            println("connect successful")
          case _ if retry == 0 =>
            println("no more retry times")
          case _ =>
            val order = (100 - retry) + 1
            val delay = 1 << order
            println(
              Clock
                .systemUTC()
                .instant() + ": connect failed ,reconnect $order times...")
            bootstrap
              .config()
              .group()
              .schedule(new Runnable {
                override def run(): Unit =
                  connect(bootstrap, host, port, retry - 1)
              }, delay, TimeUnit.SECONDS)
      })
  }

  val client = new NettyClient
  connect(client.bootstrap, "localhost", 8000, 100)
}
