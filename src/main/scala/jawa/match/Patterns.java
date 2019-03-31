package jawa.match;

import java.util.Optional;
import java.util.function.Function;


public class Patterns {
    public static <T,R> Pattern<T,R> caseOf(Class<? super T> clazz, Function<T,R> function){
        return ClassPattern.inCaseOf(clazz,function);
    }

    public static <R> Pattern<Integer,R> caseOf(int pattern, Function<Integer,R> function){
        return IntegerPattern.inCaseOf(pattern,function);
    }

    public static <R> Pattern<String,R> caseOf(String pattern, Function<String,R> function){
        return StringPattern.inCaseOf(pattern,function);
    }

    public static <R> Pattern<Boolean,R> caseOf(boolean pattern, Function<Boolean,R> function){
        return BooleanPattern.inCaseOf(pattern,function);
    }

    public static <T,R> Pattern<T,R> otherwise(Function<T,R> function){
        return OtherwisePattern.otherwise(function);
    }

    public static <T,R> Pattern<Optional<T>,R> caseOf(Optional<T> op,Function<Optional<T>,R> f){
        return OptionalPattern.inCaseOf(op,f);
    }
}
