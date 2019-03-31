package jawa;

import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.Iterator;
import java.util.Set;

/**
 * @Author: xck
 * @File: NioServer
 * @Time: 2:27 2019/3/29
 */
public class NioServer implements Runnable{
    private Selector selector;
    private ServerSocketChannel serverSocketChannel;
    public NioServer(int port) throws Exception{
        selector = Selector.open();
        serverSocketChannel = ServerSocketChannel.open();
        serverSocketChannel.configureBlocking(false);
        serverSocketChannel.register(selector, SelectionKey.OP_ACCEPT);
    }

    @Override
    public void run() {
        while(!Thread.interrupted()){
            try{
                selector.select();
                Set<SelectionKey> selected = selector.selectedKeys();
                Iterator<SelectionKey> it = selected.iterator();
                while(it.hasNext()){
                    it.remove();
                    dispatch(it.next());
                }
            }catch (Exception e){

            }
        }
    }

    private void dispatch(SelectionKey next) throws Exception {
        if(next.isAcceptable())
            register(next);
        else if(next.isReadable())
            read(next);
        else if(next.isWritable())
            wirete(next);
    }

    private void wirete(SelectionKey next) {
    }

    private void read(SelectionKey next) {
    }

    private void register(SelectionKey key) throws  Exception{
        ServerSocketChannel server = (ServerSocketChannel) key.channel();
        SocketChannel channel = server.accept();
        channel.configureBlocking(false);
        channel.register(this.selector,SelectionKey.OP_READ);
    }
}
