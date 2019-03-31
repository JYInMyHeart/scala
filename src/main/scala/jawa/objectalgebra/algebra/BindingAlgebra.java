package jawa.objectalgebra.algebra;

public interface BindingAlgebra<E> {
    E ident(String name);

    E set(String name, E value);

    E define(String name, E value);
}
