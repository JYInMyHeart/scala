package jawa.objectalgebra.algebra;

public interface EvalIntAlgebra extends IntAlgebra<IEval> {
    @Override
    default IEval intLiteral(int value){
        return (env) -> value;
    }

    @Override
    default IEval add(IEval a, IEval b){
        return (env) -> a.eval(env) + b.eval(env);
    }

    @Override
    default IEval sub(IEval a, IEval b){
        return (env) -> a.eval(env) - b.eval(env);
    }

    @Override
    default IEval mul(IEval a, IEval b){
        return (env) -> a.eval(env) * b.eval(env);
    }

    @Override
    default IEval div(IEval a, IEval b){
        return (env) -> a.eval(env) / b.eval(env);
    }
}
