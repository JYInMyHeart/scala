package jawa.objectalgebra.algebra;

public interface IntAlgebra<E> {
    E intLiteral(int value);

    E add(E a, E b);

    E sub(E a, E b);

    E mul(E a, E b);

    E div(E a, E b);
}
