package jawa.match;

import java.util.Optional;

public class OptionExtractor<T,R> extends AbsExtractor<T,R, Optional<T>> {
    @Override
    public Pattern<Optional<T>, T> returnPId() {
        return new Pattern<Optional<T>, T>() {
            @Override
            public boolean matches(Optional<T> value) {
                return value.isPresent();
            }

            @Override
            public T apply(Optional<T> value) {
                return value.get();
            }
        };
    }
}
