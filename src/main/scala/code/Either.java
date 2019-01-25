package code;

import java.util.function.Function;

public abstract class Either<L,R> {
    public abstract <T> T pm(Function<L,T> lt,Function<R,T> rt);

    static <L,R> Either<L,R> left(L l){
        return new Either<L, R>() {
            @Override
            public <T> T pm(Function<L, T> lt, Function<R, T> rt) {
                return lt.apply(l);
            }

            @Override
            public boolean equals(Object rhs){
                return rhs instanceof Either<?,?> && ((Either<?,?>) rhs).pm(l::equals,rr -> false);
            }
        };
    }

    static <L,R> Either<L,R> right(R r){
        return new Either<L, R>() {
            @Override
            public <T> T pm(Function<L, T> lt, Function<R, T> rt) {
                return rt.apply(r);
            }

            @Override
            public boolean equals(Object rhs){
                return rhs instanceof Either<?,?> && ((Either<?,?>) rhs).pm(ll -> false,r :: equals);
            }
        };
    }
}
