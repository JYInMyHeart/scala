package code;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;
public class NetClient {
    public static void main(String[] args) {
        try {
            SocketChannel socketChannel = SocketChannel.open();
            socketChannel.connect(new InetSocketAddress("localhost", 8888));

            socketChannel.configureBlocking(true);

            ByteBuffer writeBuffer = ByteBuffer.allocate(1024);
            writeBuffer.put("hello world!".getBytes());
            writeBuffer.flip();

            socketChannel.write(writeBuffer);
            socketChannel.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
