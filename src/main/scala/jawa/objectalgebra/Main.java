package jawa.objectalgebra;

public class Main {
    interface Exp {
        Value eval();
    }

    class Lit implements Exp {
        int x;

        @Override
        public Value eval() {
            return new VInt(x);
        }

        public Lit(int x) {
            this.x = x;
        }
    }

    class Add implements Exp{
        Exp l,r;

        public Add(Exp l, Exp r) {
            this.l = l;
            this.r = r;
        }

        @Override
        public Value eval() {
            return new VInt(l.eval().getInt() + r.eval().getInt());
        }
    }

    class Sub implements Exp{
        Exp l,r;

        public Sub(Exp l, Exp r) {
            this.l = l;
            this.r = r;
        }

        @Override
        public Value eval() {
            return new VInt(l.eval().getInt() - r.eval().getInt());
        }
    }

    interface Value {
        int getInt();
        boolean getBool();
    }

    class VInt implements Value{
        int x;

        public VInt(int x) {
            this.x = x;
        }

        @Override
        public int getInt() {
            return x;
        }

        @Override
        public boolean getBool() {
            return false;
        }
    }

    class VBool implements Value{
        boolean b;

        public VBool(boolean b) {
            this.b = b;
        }

        @Override
        public int getInt() {
            return 0;
        }

        @Override
        public boolean getBool() {
            return b;
        }
    }
}
