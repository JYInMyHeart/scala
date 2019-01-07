package code;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

public class TestRank {
    public static void main(String[] args) {
        List<Integer> list1 = new ArrayList<>();
        list1.add(1);
        list1.add(2);
        List<String> list2 = new ArrayList<>();
        list2.add("1");
       Function<List<Object>,Integer> f = List::size;
//        apply(f,new Tuple<List<Integer>,List<String>>(list1,list2));
    }

    static <A> Tuple<? ,? > apply(Function<List<?>,Integer> function,Tuple<List<? >,List<?>> tuple){
        return new Tuple(function.apply(tuple.t),function.apply(tuple.u) );
    }



    static class Tuple<T,U>{
        T t;
        U u;

        public Tuple(T t, U u) {
            this.t = t;
            this.u = u;
        }
    }
}
