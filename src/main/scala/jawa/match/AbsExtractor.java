package jawa.match;

import java.util.function.Function;

public abstract class AbsExtractor<T,R,F> implements Extractor<T,R,F> {
    @Override
    public Pattern<F, R> returnP(Function<T, R> function) {
        return new Pattern<F, R>() {
            @Override
            public boolean matches(F value) {
                return returnPId().matches(value);
            }

            @Override
            public R apply(F value) {
                return function.apply(returnPId().apply(value));
            }
        };
    }

    private Extractor<T,R,F> self = (Extractor<T,R,F>)this;

    @Override
    public <T2, R2> Extractor<T2, R2, F> compose(Extractor<T2, R2, T> e) {
        return new AbsExtractor<T2,R2,F>(){

            @Override
            public Pattern<F, T2> returnPId() {
                return new Pattern<F, T2>() {
                    @Override
                    public boolean matches(F value) {
                        if(self.returnPId().matches(value)){
                            T r = self.returnPId().apply(value);
                            return e.returnPId().matches(r);
                        }
                        return false;
                    }

                    @Override
                    public T2 apply(F value) {
                        return e.returnPId().apply(self.returnPId().apply(value));
                    }
                };
            }
        };
    }
}
