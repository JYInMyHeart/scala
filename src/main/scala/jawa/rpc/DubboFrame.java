package jawa.rpc;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;

public class DubboFrame {
    public static void export(final Object service, int port) throws Exception {
        assert service != null;
        assert port <= 65535 && port >= 0;
        System.out.println("Export service " + service.getClass().getName() + " on " + port);
        ServerSocketChannel serverSocketChannel = ServerSocketChannel.open();
        serverSocketChannel.bind(new InetSocketAddress(port));
        while (true) {
            try {
                SocketChannel socketChannel = serverSocketChannel.accept();
                new Thread(() -> {
                    ByteBuffer buffer = ByteBuffer.allocate(256);
                    try (ObjectInputStream input = new ObjectInputStream(socketChannel.socket().getInputStream());) {
                        String methodName = input.readUTF();
                        Class<?>[] parameterTypes = (Class<?>[]) input.readObject();
                        Object[] arguements = (Object[]) input.readObject();
                        try (ObjectOutputStream output = new ObjectOutputStream(socketChannel.socket().getOutputStream())) {
                            Method method = service.getClass().getMethod(methodName, parameterTypes);
                            Object result = method.invoke(service, arguements);
                            output.writeObject(result);
                        } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
                            System.out.println("error");
                        }
                    } catch (IOException | ClassNotFoundException e) {
                        e.printStackTrace();
                    }

                }).start();
            } catch (Exception e) {

            }
        }
    }

    @SuppressWarnings("unchecked")
    public static <T> T refer(final Class<?> interfaceClass, final String host, final int port) {
        System.out.println("Get remote service " + interfaceClass.getName() + " from server " + host + " : " + port);
        return (T) Proxy.newProxyInstance(interfaceClass.getClassLoader(), new Class<?>[]{interfaceClass}, (proxy, method, args) -> {
            try (Socket socket = new Socket(host, port)) {
                try (ObjectOutputStream outputStream = new ObjectOutputStream(socket.getOutputStream())) {
                    outputStream.writeUTF(method.getName());
                    outputStream.writeObject(method.getParameterTypes());
                    outputStream.writeObject(args);
                    ObjectInputStream input = new ObjectInputStream(socket.getInputStream());
                    try {
                        Object result = input.readObject();
                        if (result instanceof Throwable) {
                            throw (Throwable) result;
                        }
                        return result;
                    }catch (Throwable e){
                        e.printStackTrace();
                    }
                }catch (Throwable e){
                    e.printStackTrace();
                }
            }catch (Throwable e){
                e.printStackTrace();
            }
            return null;
        });
    }
}
