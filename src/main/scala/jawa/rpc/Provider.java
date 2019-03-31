package jawa.rpc;

public class Provider {
    public static void main(String[] args) throws Exception {
        Hello hello = new HelloImpl();
        DubboFrame.export(hello,12345);
    }
}
