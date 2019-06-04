package jawa.reactor;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.ByteBufAllocator;

/**
 * @Author: xck
 * @File: ByteBufDemo
 * @Time: 19:03 2019/4/21
 */
public class ByteBufDemo {
    public static void main(String[] args) {
        ByteBuf buffer = ByteBufAllocator.DEFAULT.buffer(9,100);
        print("allocate ByteBuf(9,100)",buffer);
        buffer.writeBytes(new byte[]{1,2,3,4});
        print("writeBytes(1,2,3,4",buffer);
        buffer.writeInt(12);
        print("writenInt(12)",buffer);
    }

    static void print(String action,ByteBuf buffer){
        System.out.println("after ===========" + action + "============");
        System.out.println("capacity(): " + buffer.capacity());
        System.out.println("maxCapacity(): " + buffer.maxCapacity());
        System.out.println("readerIndex(): " + buffer.readerIndex());
        System.out.println("readableBytes(): " + buffer.readableBytes());
        System.out.println("isReadable(): " + buffer.isReadable());
        System.out.println("writerIndex(): " + buffer.writerIndex());
        System.out.println("writableBytes(): " + buffer.writableBytes());
        System.out.println("isWritable(): " + buffer.isWritable());
        System.out.println("maxWritableBytes(): " + buffer.maxWritableBytes());
        System.out.println();
    }
}
