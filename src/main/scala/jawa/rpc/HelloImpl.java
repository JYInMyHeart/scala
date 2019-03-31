package jawa.rpc;

public class HelloImpl implements Hello {
    @Override
    public String hello(String name) {
        return "hello " + name;
    }
}
