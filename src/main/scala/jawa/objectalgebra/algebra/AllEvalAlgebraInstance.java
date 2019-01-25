package jawa.objectalgebra.algebra;

public class AllEvalAlgebraInstance implements AllFactory,AllAlgebra<IEval> {
    @Override
    public IEval ident(String name) {
        return null;
    }

    @Override
    public IEval set(String name, IEval value) {
        return null;
    }

    @Override
    public IEval define(String name, IEval value) {
        return null;
    }

    @Override
    public IEval intLiteral(int value) {
        return null;
    }

    @Override
    public IEval add(IEval a, IEval b) {
        return null;
    }

    @Override
    public IEval sub(IEval a, IEval b) {
        return null;
    }

    @Override
    public IEval mul(IEval a, IEval b) {
        return null;
    }

    @Override
    public IEval div(IEval a, IEval b) {
        return null;
    }
}
