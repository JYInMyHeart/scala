package jawa.match;

public interface Pattern<T,R> {
    boolean matches(T value);
    R apply(T value);
}
