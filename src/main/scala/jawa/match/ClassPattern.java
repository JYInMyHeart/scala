package jawa.match;

import java.util.function.Function;

public class ClassPattern<T,R> implements Pattern<T,R> {
    private Class<? super T> clazz;
    private Function<T, R> function;

    public ClassPattern(Class<? super T> clazz, Function<T, R> function) {
        this.clazz = clazz;
        this.function = function;
    }


    @Override
    public boolean matches(T value) {
        return clazz.isInstance(value);
    }

    @SuppressWarnings("unchecked")
    @Override
    public R apply(T value) {
        return function.apply(value);
    }

    public static <T,R> Pattern inCaseOf(Class<? super T> clazz, Function<T, R> function) {
        return new ClassPattern<>(clazz, function);
    }


}
