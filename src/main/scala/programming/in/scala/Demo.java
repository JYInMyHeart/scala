package programming.in.scala;

import java.util.function.Function;

import static java.lang.System.*;

public class Demo {
    public static void main(String[] args) {
        Function f =  a -> a;
        Function<Object, Object> t = Function.identity();

    }
}
