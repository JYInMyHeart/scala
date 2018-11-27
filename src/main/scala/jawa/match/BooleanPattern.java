package jawa.match;

import java.util.function.Function;

public class BooleanPattern<R> implements Pattern<Boolean,R> {
    private final Boolean pattern;
    private final Function<Boolean,R> function;

    public BooleanPattern(Boolean pattern, Function<Boolean, R> function) {
        this.pattern = pattern;
        this.function = function;
    }

    @Override
    public boolean matches(Boolean value) {
        return pattern.equals(value);
    }

    @Override
    public R apply(Boolean value) {
        return function.apply(value);
    }
    public static <R> Pattern<Boolean,R> inCaseOf(Boolean pattern,Function<Boolean,R> function){
        return new BooleanPattern<>(pattern,function);
    }
}
