package java;

import java.util.Deque;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Complier {
    public static void main(String[] args) {
        System.out.println(new Complier().pass1("[ x y z ] ( 2*3*x + 5*y - 3*z ) / (1 + 333 + 2*2)"));

    }

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

    interface Ast {
        String op();
    }

    class BinOp implements Ast {
        Ast a;
        String op;
        Ast b;

        public BinOp(String op,Ast a,  Ast b) {
            this.a = a;
            this.op = op;
            this.b = b;
        }

        @Override
        public String op() {
            return op;
        }

        Ast a() {
            return a;
        }

        Ast b() {
            return b;
        }

        @Override
        public String toString() {
            return "{'" + op + "':" + a + "," + b + "}";
        }
    }

    class Unop implements Ast {
        int n;
        String op;

        public Unop(String op,int n ) {
            this.n = n;
            this.op = op;
        }

        @Override
        public String op() {
            return op;
        }

        int n() {
            return n;
        }

        @Override
        public String toString() {
            return "{'" + op + "':" + n + "}";
        }
    }

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
            if(c.equals(token)){
                next();
                return true;
            }
            return false;
        }


        public Ast parseFunc(){
            next();
            eat("[");
            parseArgs();
            eat("]");
            return parseExpression();
        }

        void parseArgs(){
            while(c.matches("[a-zA-Z]+")){
                next();
            }
        }

        Ast parseExpression(){
            Ast x = parseTerm();
            while (true) {
                if (eat("+")) {
                    Ast rhs = parseTerm();
                    x =  new BinOp("+",x,rhs);
                }
                else if(eat("-")) {
                    Ast rhs = parseTerm();
                    x =  new BinOp("-",x,rhs);
                }
                else  return x;
            }
        }
        Ast parseTerm(){
            Ast x = parseFactor();
            while (true) {
                if (eat("*")) {
                    Ast rhs = parseFactor();
                    x = new BinOp("*",x,rhs);
                }
                else if(eat("/")) {
                    Ast rhs = parseFactor();
                    x =  new BinOp("/",x,rhs);
                }
                else  return x;
            }
        }

        Ast parseFactor(){
            if(c.matches("\\d+")){
                Ast r = new Unop("imm",Integer.valueOf(c));
                next();
                return r;
            }else if(c.matches("[a-zA-Z]+")){
                Ast r = new Unop("arg",argOffset);
                argOffset++;
                next();
                return r;
            }else{
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
        return p.parseFunc();
    }

    /**
     * Returns an AST with constant expressions reduced
     */
    public Ast pass2(Ast ast) {
        return null;
    }

    /**
     * Returns assembly instructions
     */
    public List<String> pass3(Ast ast) {
        return null;
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
