package java;

public abstract class Nat {
    abstract class Exp {
        abstract <T> T accept(ExpVisitor<T> visitor);
    }

    final class Literal extends Exp {
        public final int val;

        public Literal(int val) {
            this.val = val;
        }

        @Override
        <T> T accept(ExpVisitor<T> visitor) {
            return visitor.forLiteral(val);
        }
    }

    final class Add extends Exp {
        public final Exp a;
        public final Exp b;

        public Add(Exp a, Exp b) {
            this.a = a;
            this.b = b;
        }

        @Override
        <T> T accept(ExpVisitor<T> visitor) {
            return visitor.forAdd(a, b);
        }
    }

    final class Subtract extends Exp {
        public final Exp a;
        public final Exp b;

        public Subtract(Exp a, Exp b) {
            this.a = a;
            this.b = b;
        }

        @Override
        <T> T accept(ExpVisitor<T> visitor) {
            return visitor.forSubtract(a, b);
        }
    }

    final class Multiply extends Exp {
        public final Exp a;
        public final Exp b;

        public Multiply(Exp a, Exp b) {
            this.a = a;
            this.b = b;
        }

        @Override
        <T> T accept(ExpVisitor<T> visitor) {
            return visitor.forMultiply(a, b);
        }
    }

    class ExpEvalVisitor implements ExpVisitor<Integer> {
        public Integer forLiteral(int v) {
            return v;
        }

        public Integer forAdd(Exp a, Exp b) {
            return a.accept(this) + b.accept(this);
        }

        public Integer forSubtract(Exp a, Exp b) {
            return a.accept(this) - b.accept(this);
        }

        public Integer forMultiply(Exp a, Exp b) {
            return a.accept(this) * b.accept(this);
        }
    }

    class ExpShowVistor implements ExpVisitor<String> {


        @Override
        public String forLiteral(int v) {
            return v + "";
        }

        @Override
        public String forAdd(Exp a, Exp b) {
            return "(" + a.accept(this) + "+" + b.accept(this) + ")";
        }

        @Override
        public String forSubtract(Exp a, Exp b) {
            return "(" + a.accept(this) + "-" + b.accept(this) + ")";
        }

        @Override
        public String forMultiply(Exp a, Exp b) {
            return "(" + a.accept(this) + "*" + b.accept(this) + ")";
        }
    }

    interface ExpVisitor<T> {
        T forLiteral(int v);

        T forAdd(Exp a, Exp b);

        T forSubtract(Exp a, Exp b);

        T forMultiply(Exp a, Exp b);
    }


}

class Zero extends Nat {
}

class OneMoreThan extends Nat {
    Nat predecessorl;

    OneMoreThan(Nat p) {
        predecessorl = p;
    }
}





