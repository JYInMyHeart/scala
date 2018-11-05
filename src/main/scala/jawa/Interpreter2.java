package jawa;

import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Interpreter2 {

    public Double input(String input) {
        Deque<String> tokens = tokenize(input);
        return null;
    }

    private static Deque<String> tokenize(String input) {
        Deque<String> tokens = new LinkedList<>();
        Pattern pattern = Pattern.compile("=>|[-+*/%=\\(\\)]|[A-Za-z_][A-Za-z0-9_]*|[0-9]*(\\.?[0-9]+)");
        Matcher m = pattern.matcher(input);
        while (m.find()) {
            tokens.add(m.group());
        }
        return tokens;
    }

    class Parser {
        private Map<String,Double> map = new HashMap<>();
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
            } else if (c.matches("[A-Za-z_][A-Za-z0-9_]*")) {
                while (true){
                    if(c.matches("[A-Za-z_][A-Za-z0-9_]*"){
                        String v1 = c;
                        next();
                        if(c.equals("=")){
                            next();
                            map.put(v1, Double.valueOf(c));
                        }
                    }
                }

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

}