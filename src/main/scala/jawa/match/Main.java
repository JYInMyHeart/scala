package jawa.match;

import java.util.Optional;

import static jawa.match.Bind.let;
import static jawa.match.OtherwisePattern.otherwise;
import static jawa.match.Patterns.caseOf;

public class Main {
    public static void main(String[] args) {
        let(1).match(
                caseOf(1, x -> x + 1),
                caseOf(2, x -> x * 2),
                otherwise(x -> x)
        ).forEach(System.out::println);
        System.out.println(fact(5));

        Bind.<Number>let(1).match(
                caseOf(Integer.class,x -> x + 1),
                caseOf(Double.class,x -> x + 1.0),
                otherwise(x -> x)
        ).forEach(System.out::println);

        let(Optional.of(1)).match(
                caseOf(Optional.of(1), Optional::get),
                caseOf(Optional.empty(),__ -> 0),
                otherwise(x -> Integer.MIN_VALUE)
        ).forEach(System.out::println);
    }

    public static int fact(int n) {
        return let(n).match(
                caseOf(0, __ -> 1),
                otherwise(__ -> n * fact(n - 1))
        ).var;
    }


}
