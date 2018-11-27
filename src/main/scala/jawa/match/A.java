package jawa.match;

import static jawa.match.Bind.let;
import static jawa.match.Patterns.caseOf;
import static jawa.match.Patterns.otherwise;

public abstract class A {

    @Override
    public String toString() {
        return "A{}";
    }

    public static class B extends A{
        @Override
        public String toString() {
            return "B{...}";
        }
    }

    public static class C extends A{
        @Override
        public String toString() {
            return "C{...}";
        }
    }

    public static void main(String[] args) {
        let((A)new B()).match(
                caseOf(B.class,B::toString),
                caseOf(C.class,C::toString),
                otherwise(x -> "other")
        ).forEach(System.out::println);
    }
}
