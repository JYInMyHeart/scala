package jawa.rpc;

public class Consumer {
    public static void main(String[] args) throws InterruptedException {
        Hello hello = DubboFrame.refer(Hello.class,"localhost",12345);
        for(int i = 0;i < 100;i++){
            String h = hello.hello("World " + i);
            System.out.println(h);
            Thread.sleep(500);
        }
    }
}
