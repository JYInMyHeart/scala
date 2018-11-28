package jawa.match;

import java.util.function.Function;

public interface Extractor<T, R, F> {
    Pattern<F,R> returnP(Function<T,R> function);
    Pattern<F,T> returnPId();
    <T2,R2> Extractor<T2,R2,F> compose(Extractor<T2,R2,T> e);
}
