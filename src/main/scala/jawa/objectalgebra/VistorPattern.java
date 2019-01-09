package jawa.objectalgebra;

public class VistorPattern {
    interface Exp{
        <A> A accept(IntAlg<A> vis);
    }

    interface IntAlg<A>{
        A lit(int a);
        A add(A a,A b);

    }

    interface SubIntAlg<A> extends IntAlg<A>{
        A sub(A a,A b);
    }

    interface IPrint{
        String print();
    }

    class Printer implements IntAlg<IPrint>{

        @Override
        public IPrint lit(int a) {
            return () -> String.valueOf(a);
        }

        @Override
        public IPrint add(IPrint iPrint, IPrint b) {
            return () -> iPrint.print() + b.print();
        }

    }

    class SubPrinter extends Printer implements SubIntAlg<IPrint>{

        @Override
        public IPrint sub(IPrint iPrint, IPrint b) {
            return () -> iPrint.print() + b.print();
        }
    }

    class Lit implements Exp{
        private int value;

        public Lit(int value) {
            this.value = value;
        }

        @Override
        public <A> A accept(IntAlg<A> vis) {
            return vis.lit(value);
        }
    }

    class Add implements Exp{
        Exp l,r;

        public Add(Exp l, Exp r) {
            this.l = l;
            this.r = r;
        }

        @Override
        public <A> A accept(IntAlg<A> vis) {
            return vis.add(l.accept(vis),r.accept(vis));
        }
    }


}
