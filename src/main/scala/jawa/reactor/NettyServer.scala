package jawa.reactor

import java.nio.charset.Charset
import java.time.Clock

import io.netty.bootstrap.ServerBootstrap
import io.netty.buffer.ByteBuf
import io.netty.channel.{
  ChannelHandlerContext,
  ChannelInboundHandlerAdapter,
  ChannelInitializer
}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.{NioServerSocketChannel, NioSocketChannel}
import io.netty.util.concurrent.{Future, GenericFutureListener}

class NettyServer {
  val bossGroup = new NioEventLoopGroup
  val workerGroup = new NioEventLoopGroup
  val serverBootStrap = new ServerBootstrap
  serverBootStrap
    .group(bossGroup, workerGroup)
    .channel(classOf[NioServerSocketChannel])
    .childHandler(new ChannelInitializer[NioSocketChannel] {
      override def initChannel(ch: NioSocketChannel): Unit = {
        println("server start...")
        ch.pipeline().addLast(new FirstServerHandler())
      }
    })
}

class FirstServerHandler extends ChannelInboundHandlerAdapter {
  override def channelRead(ctx: ChannelHandlerContext, msg: Any): Unit = {
    val byteBuf: ByteBuf = msg.asInstanceOf[ByteBuf]
    println(s"${Clock.systemUTC().instant()}: server read the data -> ${byteBuf
      .toString(Charset.forName("utf-8"))}")

    println(s"${Clock.systemUTC().instant()}: server write the data -> ")
    val out = getByteBuf(ctx)
    val tmp = out.duplicate()
//    out.retain()
    ctx.channel().writeAndFlush(out)

//    out.release()
    println(tmp.array().length)
  }

  private def getByteBuf(ctx:ChannelHandlerContext): ByteBuf ={
    val bytes = "hello this is server.".getBytes(Charset.forName("utf-8"))
    val buffer = ctx.alloc().buffer()
    buffer.writeBytes(bytes)
    buffer
  }
}

object Stater extends App {
  def bind(serverBootstrap: ServerBootstrap, port: Int): Unit = {
    serverBootstrap
      .bind(port)
      .addListener((future: Future[_ >: Void]) =>
        future.isSuccess match {
          case true => println(s"port ${port} successful")
          case _ =>
            println(s"port ${port} failed")
            bind(serverBootstrap, port + 1)
      })
  }
  val nettyServer = new NettyServer
  bind(nettyServer.serverBootStrap, 8000)

}
