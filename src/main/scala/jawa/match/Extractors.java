package jawa.match;

import java.util.Optional;
import java.util.function.Function;

public class Extractors {
    public static <T,R> Extractor<T,R, Optional<T>> some(){return new OptionExtractor<>();}
    public static <T, R, T2, R2> Pattern<Optional<T>, R2> some(Extractor<T2, R2, T> e, Function<T2, R2> f) {
        return new OptionExtractor<T, R>().compose(e).returnP(f);
    }

    public static <T, R, T2, R2> Extractor<T2, R2, Optional<T>> some(Extractor<T2, R2, T> e) {
        return new OptionExtractor<T, R>().compose(e);
    }

    public static <T, R> Pattern<Optional<T>, R> some(Function<T, R> f) { return new OptionExtractor<T, R>().returnP(f); }



}
