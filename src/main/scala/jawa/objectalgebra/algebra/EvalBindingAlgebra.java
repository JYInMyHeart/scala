package jawa.objectalgebra.algebra;

public interface EvalBindingAlgebra extends BindingAlgebra<IEval> {
    @Override
    default IEval ident(String name){
        return (env) -> env.find(name);
    }

    @Override
    default IEval set(String name, IEval value){
        return env -> {
            env.set(name,value.eval(env));
            return 0;
        };
    }

    @Override
    default IEval define(String name, IEval value){
        return env -> {
            env.define(name,value.eval(env));
            return 0;
        };
    }
}
