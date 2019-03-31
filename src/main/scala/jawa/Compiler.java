package jawa;

import java.util.ArrayList;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Compiler {
    public static void main(String[] args) {
        Compiler c = new Compiler();
        //IM 7,SW,AR 2,PU
//        Ast ast = c.pass1("[ x y z ] ( 2*3*x + 5*y - 3*z ) / (1  + 2*2 + 3)");
//        System.out.println(Simulator.simulate(new Compiler().compile("[ x y z ] ( 2*3*x + 5*y - 3*z ) / (1  + 2*2 + 3)"),4,0,0));
//        Ast ast1 = c.pass1("[ x y z ] x - y - z + 10 / 5 / 2 - 7 / 1 / 7");
        Ast ast1 = c.pass1("[] 1+2");
//        Ast ast1 = c.pass1("[ x ] x + 2*5");
        System.out.println(ast1);
//        Ast ast2 = c.pass2(ast1);
//        System.out.println(ast2);
//        List<String> ast3 = c.pass3(ast2);
//        System.out.println(ast3);
//        System.out.println(Simulator.simulate(ast3,5,4,1));

    }
    //[IM 8, PU, AR 2, SW, IM 3, MU, PU, AR 1, SW, IM 5, MU, PU, AR 0, SW, IM 6, MU, SW, PO, AD, SW, PO, SU, SW, PO, DI]
    //[IM 8, PU, AR 0, SW, IM 6, MU, PU, AR 1, SW, IM 5, MU, SW, PO, AD, PU, AR 2, SW, IM 3, MU, SW, PO, SU, SW, PO, DI]

    public List<String> compile(String prog) {
        return pass3(pass2(pass1(prog)));
    }

    /*
    function   ::= '[' arg-list ']' expression

    arg-list   ::= *//* nothing *//*
            | variable arg-list

    expression ::= term
                 | expression '+' term
                 | expression '-' term

    term       ::= factor
                 | term '*' factor
                 | term '/' factor

    factor     ::= number
                 | variable
                 | '(' expression ')'
                 */


    class Parser {
        private Deque<String> tokens;
        private int pos;
        private String c;
        private int argOffset;

        public Parser(Deque<String> tokens) {
            this.tokens = tokens;
            pos = 0;
            argOffset = 0;
        }

        public void next() {
            c = tokens.pop();
            pos++;
        }

        public boolean eat(String token) {
            if (c.equals(token)) {
                next();
                return true;
            }
            return false;
        }


        public Ast parseFunc() {
            next();
            eat("[");
            parseArgs();
            eat("]");
            return parseExpression();
        }

        public void parseArgs() {
            while (c.matches("[a-zA-Z]+")) {
                next();
            }
        }

        Ast parseExpression() {
            Ast x = parseTerm();
            while (true) {
                if (eat("+")) {
                    Ast rhs = parseTerm();
                    x = new BinOp("+", x, rhs);
                } else if (eat("-")) {
                    Ast rhs = parseTerm();
                    x = new BinOp("-", x, rhs);
                } else return x;
            }
        }

        Ast parseTerm() {
            Ast x = parseFactor();
            while (true) {
                if (eat("*")) {
                    Ast rhs = parseFactor();
                    x = new BinOp("*", x, rhs);
                } else if (eat("/")) {
                    Ast rhs = parseFactor();
                    x = new BinOp("/", x, rhs);
                } else return x;
            }
        }

        Ast parseFactor() {
            if (c.matches("\\d+")) {
                Ast r = new UnOp("imm", Integer.valueOf(c));
                next();
                return r;
            } else if (c.matches("[a-zA-Z]+")) {
                Ast r = new UnOp("arg", argOffset);
                argOffset++;
                next();
                return r;
            } else {
                eat("(");
                Ast expr = parseExpression();
                eat(")");
                return expr;
            }
        }
    }


    /**
     * Returns an un-optimized AST
     */
    public Ast pass1(String prog) {
        Deque<String> tokens = tokenize(prog);
        Parser p = new Parser(tokens);
        Ast ast = p.parseFunc();
        return ast;
    }

    /**
     * Returns an AST with constant expressions reduced
     */
    public Ast pass2(Ast ast) {
        int len = 0;
        while(ast.toString().length() != len){
            len = ast.toString().length();
            ast = cal(ast);
        }
        return ast;
    }

    public Ast cal(Ast ast) {
        if (ast instanceof BinOp) {
            if (((BinOp) ast).a() instanceof UnOp
                    && ((BinOp) ast).b() instanceof UnOp) {
                ast = getAstValue(ast.op(),((BinOp) ast).a(), ((BinOp) ast).b());
            } else if (((BinOp) ast).a() instanceof UnOp) {
                ast = new BinOp(ast.op(),((BinOp) ast).a(), cal(((BinOp) ast).b()));
            } else if (((BinOp) ast).b() instanceof UnOp) {
                ast = new BinOp(ast.op(),cal(((BinOp) ast).a()), ((BinOp) ast).b());
            } else {
                ast = new BinOp(ast.op(),cal(((BinOp) ast).a()), cal(((BinOp) ast).b()));
            }
        }
        return ast;
    }

    private Ast getAstValue(String op,Ast ast1, Ast ast2) {
        Ast ast = null;
        if (((UnOp) ast1).op.equals("imm")
                && ((UnOp) ast2).op.equals("imm")) {
            switch (op) {
                case "+":
                    ast = new UnOp("imm",
                            ((UnOp) ast1).n() +
                                    ((UnOp) ast2).n());
                    return ast;
                case "-":
                    ast = new UnOp("imm",
                            ((UnOp) ast1).n() -
                                    ((UnOp) ast2).n());
                    return ast;
                case "*":
                    ast = new UnOp("imm",
                            ((UnOp) ast1).n() *
                                    ((UnOp) ast2).n());
                    return ast;
                case "/":
                    if (((UnOp) ast2).n() != 0) {
                        ast = new UnOp("imm",
                                ((UnOp) ast1).n() /
                                        ((UnOp) ast2).n());
                        return ast;
                    }else{
                        throw new RuntimeException("can't divide zero!!!");
                    }
            }
        }
        ast = new BinOp(op,ast1,ast2);
        return ast;
    }


    /*
    "IM n"     // load the constant value n into R0
    "AR n"     // load the n-th input argument into R0
    "SW"       // swap R0 and R1
    "PU"       // push R0 onto the stack
    "PO"       // pop the top value off of the stack into R0
    "AD"       // add R1 to R0 and put the result in R0
    "SU"       // subtract R1 from R0 and put the result in R0
    "MU"       // multiply R0 by R1 and put the result in R0
    "DI"       // divide R0 by R1 and put the result in R0
    * */

    /*
    *   new BinOp("+", new UnOp("arg", 0), new UnOp("imm", 10))
    *   [ "IM 10", "SW", "AR 0", "AD" ]
    *   ( 2*3*x + 5*y - 3*z ) / (1  + 2*2 + 3)
    * */

    void p3(Ast ast,List<String> ins){
        if(ast instanceof BinOp){
            switch (ast.op()){
                case "+" : ins.add(0,"AD");break;
                case "-" : ins.add(0,"SU");break;
                case "*" : ins.add(0,"MU");break;
                case "/" : ins.add(0,"DI");break;
            }
            Ast a = ((BinOp) ast).a();
            Ast b = ((BinOp) ast).b();
            if(a instanceof UnOp && b instanceof UnOp){
                ins.add(0,"SW");
                p3(b,ins);
                ins.add(0,"SW");
                p3(a,ins);
            }else{
                ins.add(0,"PO");
                ins.add(0,"SW");
                p3(b,ins);
                ins.add(0,"PU");
                p3(a,ins);

            }
        }else if(ast instanceof UnOp){
            switch (ast.op()){
                case "arg":ins.add(0,"AR " + ((UnOp) ast).n());break;
                case "imm":ins.add(0,"IM " + ((UnOp) ast).n());break;
            }
        }
    }

    /**
     * Returns assembly instructions
     */
    public List<String> pass3(Ast ast) {
        List<String> ins = new ArrayList<>();
        p3(ast,ins);
        return ins;
    }

    private static Deque<String> tokenize(String prog) {
        Deque<String> tokens = new LinkedList<>();
        Pattern pattern = Pattern.compile("[-+*/()\\[\\]]|[a-zA-Z]+|\\d+");
        Matcher m = pattern.matcher(prog);
        while (m.find()) {
            tokens.add(m.group());
        }
        tokens.add("$"); // end-of-stream
        return tokens;
    }


}







interface Ast {
    String op();
}

class BinOp implements Ast {
    Ast a;
    String op;
    Ast b;

    public BinOp(String op, Ast a, Ast b) {
        this.a = a;
        this.op = op;
        this.b = b;
    }

    @Override
    public String op() {
        return op;
    }

    public Ast a() {
        return a;
    }

    public Ast b() {
        return b;
    }

    @Override
    public String toString() {
        return "{'op':'" + op + "','a':" + a + ",'b':" + b + "}";
    }


}

class UnOp implements Ast {
    int n;
    String op;

    public UnOp(String op, int n) {
        this.n = n;
        this.op = op;
    }

    @Override
    public String op() {
        return op;
    }

    public int n() {
        return n;
    }

    @Override
    public String toString() {
        return "{'op':'" + op + "','n':" + n + "}";
    }
}


class Simulator {
    public static int simulate(List<String> asm, int... argv) {
        int r0 = 0;
        int r1 = 0;
        Deque<Integer> stack = new LinkedList<>();
        for (String ins : asm) {
            String code = ins.replaceAll("\\s+[0-9]+", "");
            switch (code) {
                case "IM": r0 = Integer.parseInt(ins.substring(2).trim()); break;
                case "AR": r0 = argv[Integer.parseInt(ins.substring(2).trim())]; break;
                case "SW": int tmp = r0; r0 = r1; r1 = tmp; break;
                case "PU": stack.addLast(r0); break;
                case "PO": r0 = stack.removeLast(); break;
                case "AD": r0 += r1; break;
                case "SU": r0 -= r1; break;
                case "MU": r0 *= r1; break;
                case "DI": r0 /= r1; break;
            }
        }
        return r0;
    }
}
