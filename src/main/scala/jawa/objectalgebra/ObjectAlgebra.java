package jawa.objectalgebra;

public class ObjectAlgebra {
    interface ExpAlg<E>{
        E lit(int x);
        E add(E e1,E e2);
    }
    interface SubExpAlg<E> extends ExpAlg<E>{
        E sub(E e1,E e2);
    }

    interface Eval{
        int eval();
    }

    class EvalExpAlg implements ExpAlg<Eval>{

        @Override
        public Eval lit(int x) {
            return () -> x;
        }

        @Override
        public Eval add(Eval e1, Eval e2) {
            return () -> e1.eval() + e2.eval();
        }
    }

    class EvalSubExpAlg extends EvalExpAlg implements SubExpAlg<Eval>{

        @Override
        public Eval sub(Eval e1, Eval e2) {
            return () -> e1.eval() - e2.eval();
        }
    }
}
