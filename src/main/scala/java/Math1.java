package java;

import java.util.ArrayList;
import java.util.Iterator;

public class Math1 {
    public static void main(String[] args) throws Exception {
        Math1 b = new Math1();
        MathEvaluator a = b.new MathEvaluator();
        Exp e = a.calculate("(1+2) / 4+-5");
        System.out.println(e);
        System.out.println(b.cal(e));
    }
    enum TokenType {
        NUM, PLUS, MINUS, TIMES, DIVIDE, LPAREN, RPAREN
    }

    class Token {
        final TokenType type;
        final String value;

        Token(TokenType type) {
            this.type = type;
            this.value = "";
        }

        Token(TokenType type, String value) {
            this.type = type;
            this.value = value;
        }
    }

    class MathEvaluator {

        private Iterator<Token> tokenIter;
        private Token curToken;
        private Token nextToken;

        private void scan(String input) {
            ArrayList<Token> tokens = new ArrayList<>();
            for (int i = 0; i < input.length(); ++i) {
                char c = input.charAt(i);
                if (c == '+') { tokens.add(new Token(TokenType.PLUS)); }
                else if (c == '-') { tokens.add(new Token(TokenType.MINUS)); }
                else if (c == '*') { tokens.add(new Token(TokenType.TIMES)); }
                else if (c == '/') { tokens.add(new Token(TokenType.DIVIDE)); }
                else if (c == '(') { tokens.add(new Token(TokenType.LPAREN)); }
                else if (c == ')') { tokens.add(new Token(TokenType.RPAREN)); }
                else if (Character.isDigit(c) || c == '.') {
                    int j = i+1;
                    while (j < input.length() && (Character.isDigit(input.charAt(j)) || input.charAt(j) == '.')) ++j;
                    tokens.add(new Token(TokenType.NUM, input.substring(i, j)));
                    i = j - 1;
                } else if (!Character.isWhitespace(c)) {
                    throw new Error(String.format("Unknown token at position %d: %c", i, c));
                }
            }

            tokenIter = tokens.iterator();
        }

        private void advance() {
            curToken = nextToken;
            nextToken = tokenIter.hasNext() ? tokenIter.next() : null;
        }

        private boolean accept(TokenType tk) {
            if (nextToken != null && nextToken.type == tk) {
                advance();
                return true;
            }
            return false;
        }

        private void expect(TokenType tk) {
            if (!accept(tk)) {
                throw new Error(String.format("Expected token type: %s, but token <%s, %s> occurred",
                        tk, nextToken.type, nextToken.value));
            }
        }

        private Exp expr() {
            Exp val = term();
            while (accept(TokenType.PLUS) || accept(TokenType.MINUS)) {
                TokenType type = curToken.type;
                Exp rhs = term();
                if (type == TokenType.PLUS) val = new Plus(val,rhs);
                else val = new Sub(val,rhs);
            }
            return val;
        }

        private Exp term() {
            Exp val = factor();
            while (accept(TokenType.TIMES) || accept(TokenType.DIVIDE)) {
                TokenType type = curToken.type;
                Exp rhs = factor();
                if (type == TokenType.TIMES) val = new Mul(val,rhs);
                else val = new Div(val,rhs);
            }
            return val;
        }

        private Exp factor() {
            if (accept(TokenType.MINUS)) {
                return new Negive(pfactor());
            } else {
                return pfactor();
            }
        }

        private Exp pfactor() {
            if (accept(TokenType.NUM)) {
                return new Val(Double.valueOf(curToken.value));
            } else if (accept(TokenType.LPAREN)) {
                Exp val = expr();
                expect(TokenType.RPAREN);
                return val;
            } else {
                throw new Error(String.format("Expected NUM or LPAREN, but token <%s, %s> occurred",
                        nextToken.type, nextToken.value));
            }
        }

        public Exp calculate(String expression) {
            scan(expression);
            advance();
            return expr();
        }
    }


    abstract class Exp {
    }

    class Plus extends Exp {
        public Exp left;
        public Exp right;


        public Plus(Exp left, Exp right) {
            this.left = left;
            this.right = right;
        }

        @Override
        public String toString() {
            return "Plus (" + left.toString() + " " + right.toString() + ")";
        }
    }

    class Div extends Exp {
        Exp left;
        Exp right;


        public Div(Exp left, Exp right) {
            this.left = left;
            this.right = right;
        }
        @Override
        public String toString() {
            return "Div (" + left.toString() + " " + right.toString() + ")";
        }
    }

    class Sub extends Exp {
        Exp left;
        Exp right;


        public Sub(Exp left, Exp right) {
            this.left = left;
            this.right = right;
        }
        @Override
        public String toString() {
            return "Sub (" + left.toString() + " " + right.toString() + ")";
        }
    }

    class Mul extends Exp {
        Exp left;
        Exp right;


        public Mul(Exp left, Exp right) {
            this.left = left;
            this.right = right;
        }
        @Override
        public String toString() {
            return "Mul (" + left.toString() + " " + right.toString() + ")";
        }
    }

    class Negive extends Exp{
        Exp val;

        public Negive(Exp val) {
            this.val = val;
        }
        @Override
        public String toString() {
            return "Negive (" + val.toString() + ")";
        }
    }

    class Val extends Exp {
        double value;

        public Val(double value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return "Val (" + value + ")";
        }
    }

    public double cal(Exp e) throws Exception {
        if(e instanceof Plus){
            return cal(((Plus) e).left ) + cal(((Plus) e).right);
        }else if(e instanceof Sub){
            return cal(((Sub) e).left ) - cal(((Sub) e).right);
        }
        else if(e instanceof Div){
            return cal(((Div) e).left ) / cal(((Div) e).right);
        }
        else if(e instanceof Mul){
            return cal(((Mul) e).left ) * cal(((Mul) e).right);
        }else if(e instanceof Negive){
            return - cal(((Negive) e).val);
        }else if(e instanceof Val){
            return ((Val) e).value;
        }else{
            throw new Exception("unknown type");
        }
    }



}
