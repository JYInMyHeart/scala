package jawa.match;

import java.util.function.Consumer;

public class Bind<T> {
    public final T var;

    public Bind(T var) {
        this.var = var;
    }

    public void forEach(Consumer<T> f){
        f.accept(var);
    }

    @SafeVarargs
    final public <R> Bind<R> match(Pattern<? extends T,? extends R>... patterns){
        return Bind.let(PatternMatching.create(patterns).matchFor(var));
    }

    public static <T> Bind<T> let(T var){
        return new Bind<>(var);
    }
}
