package java;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.PushbackInputStream;

public class MathEvaluator1 {

    private PushbackInputStream pushbackInputStream;
    private int offset;
    private char ccc;

    public MathEvaluator1(String ex) {
        this.pushbackInputStream = new PushbackInputStream(new BufferedInputStream(System.in));
        offset = 0;
        ccc = ' ';
    }

    public static void main(String[] args) {
        String ex = "";
        MathEvaluator1 m1 = new MathEvaluator1(ex);
        m1.parse();
    }

    public Exp parse() {
        return parseExp();
    }

    public char getNextCh() {
        try {
            ccc = (char) pushbackInputStream.read();
            offset += 1;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return ccc;
    }

    public void back(char c) {
        try {
            pushbackInputStream.unread(c);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void space() {
        while (ccc == ' '
                || ccc == '\n'
                || ccc == '\t'
                || ccc == '\r'
        ) {
            getNextCh();
        }
        back(ccc);
    }

    // expr ::==  expr + expr | expr
    public Exp parseExp() {
        space();
        Exp e1 = parseMul();
        Exp e2 = parseExp1();
        if (e2 == null)
            return e1;
        else if (e2 instanceof Plus) {
            return ((Plus) e2).append(e1);
        } else if (e2 instanceof Sub) {
            return ((Sub) e2).append(e1);
        } else {
            throw new NullPointerException();
        }


    }

    public Exp parseMul() {
        space();
        Exp e1 = parseNum();
        Exp e2 = parseMul1();
        if (e2 == null)
            return e1;
        else if (e2 instanceof Div) {
            return ((Div) e2).append(e1);
        } else if (e2 instanceof Mul) {
            return ((Mul) e2).append(e1);
        } else {
            return null;
        }
    }

    public Exp parseExp1() {
        space();
        if (ccc == '+') {
            getNextCh();
            Exp e1 = parseMul();
            Exp e2 = parseExp1();
            if (e2 == null) {
                return new Plus(e2, e1);
            } else if (e2 instanceof Plus) {
                return ((Plus) e2).append(new Plus(null, e1));
            } else if (e2 instanceof Sub) {
                return ((Sub) e2).append(new Sub(null, e1));
            } else {
                return null;
            }
        } else if (ccc == '-') {
            getNextCh();
            Exp e1 = parseMul();
            Exp e2 = parseExp1();
            if (e2 == null) {
                return new Sub(e2, e1);
            } else if (e2 instanceof Plus) {
                return ((Plus) e2).append(new Plus(null, e1));
            } else if (e2 instanceof Sub) {
                return ((Sub) e2).append(new Sub(null, e1));
            } else {
                return null;
            }
        }
        return null;
    }

    public Exp parseMul1() {
        space();
        if (ccc == '*') {
            getNextCh();
            Exp e1 = parseNum();
            Exp e2 = parseMul1();
            if (e2 == null) {
                return new Mul(e2, e1);
            } else if (e2 instanceof Div) {
                return ((Div) e2).append(new Div(null, e1));
            } else if (e2 instanceof Mul) {
                return ((Mul) e2).append(new Mul(null, e1));
            } else {
                return null;
            }
        } else if (ccc == '/') {
            getNextCh();
            Exp e1 = parseNum();
            Exp e2 = parseMul1();
            if (e2 == null) {
                return new Div(e2, e1);
            } else if (e2 instanceof Div) {
                return ((Div) e2).append(new Div(null, e1));
            } else if (e2 instanceof Mul) {
                return ((Mul) e2).append(new Mul(null, e1));
            } else {
                return null;
            }
        }
        return null;
    }

    public Exp parseNum() {
        space();
        if (ccc == '(') {
            getNextCh();
            Exp e = parseExp();
            if (ccc == ')') {
                getNextCh();
                return e;
            }
        } else {
            return new Val(getNum());
        }
        return null;
    }

    private double getNum() {
        StringBuilder s = new StringBuilder();
        while (ccc >= '0' && ccc <= '9') {
            s.append(ccc);
            getNextCh();
        }
        back(ccc);
        return Double.valueOf(s.toString());
    }


    abstract class Exp {
    }

    class Plus extends Exp {
        public Exp left;
        public Exp right;

        public Plus append(Exp e) {
            this.left = e;
            return this;
        }

        public Plus(Exp left, Exp right) {
            this.left = left;
            this.right = right;
        }
    }

    class Div extends Exp {
        Exp left;
        Exp right;

        public Div append(Exp e) {
            this.left = e;
            return this;
        }

        public Div(Exp left, Exp right) {
            this.left = left;
            this.right = right;
        }
    }

    class Sub extends Exp {
        Exp left;
        Exp right;

        public Sub append(Exp e) {
            this.left = e;
            return this;
        }

        public Sub(Exp left, Exp right) {
            this.left = left;
            this.right = right;
        }
    }

    class Mul extends Exp {
        Exp left;
        Exp right;

        public Mul append(Exp e) {
            this.left = e;
            return this;
        }

        public Mul(Exp left, Exp right) {
            this.left = left;
            this.right = right;
        }
    }

    class Val extends Exp {
        double value;

        public Val(double value) {
            this.value = value;
        }
    }


    public double calculate(String expression) {
        return 0.0;
    }

    public static double eval(final String str) {
        return new Object() {
            int pos = -1, ch;

            void nextChar() {
                ch = (++pos < str.length()) ? str.charAt(pos) : -1;

            }

            boolean eat(int charToEat) {
                while (ch == ' ') nextChar();
                if (ch == charToEat) {
                    nextChar();
                    return true;
                }
                return false;
            }

            double parse() {
                nextChar();
                double x = parseExpression();
                if (pos < str.length()) throw new RuntimeException("Unexpected: " + (char) ch);
                return x;
            }

            // Grammar:
            // expression = term | expression `+` term | expression `-` term
            // term = factor | term `*` factor | term `/` factor
            // factor = `+` factor | `-` factor | `(` expression `)`
            //        | number | functionName factor | factor `^` factor

            double parseExpression() {
                double x = parseTerm();
                for (; ; ) {
                    if (eat('+')) x += parseTerm(); // addition
                    else if (eat('-')) x -= parseTerm(); // subtraction
                    else return x;
                }
            }

            double parseTerm() {
                double x = parseFactor();
                for (; ; ) {
                    if (eat('*')) x *= parseFactor(); // multiplication
                    else if (eat('/')) x /= parseFactor(); // division
                    else return x;
                }
            }

            double parseFactor() {
                if (eat('+')) return parseFactor(); // unary plus
                if (eat('-')) return -parseFactor(); // unary minus

                double x;
                int startPos = this.pos;
                if (eat('(')) { // parentheses
                    x = parseExpression();
                    eat(')');
                } else if ((ch >= '0' && ch <= '9') || ch == '.') { // numbers
                    while ((ch >= '0' && ch <= '9') || ch == '.') nextChar();
                    x = Double.parseDouble(str.substring(startPos, this.pos));
                } else if (ch >= 'a' && ch <= 'z') { // functions
                    while (ch >= 'a' && ch <= 'z') nextChar();
                    String func = str.substring(startPos, this.pos);
                    x = parseFactor();
                    if (func.equals("sqrt")) x = Math.sqrt(x);
                    else if (func.equals("sin")) x = Math.sin(Math.toRadians(x));
                    else if (func.equals("cos")) x = Math.cos(Math.toRadians(x));
                    else if (func.equals("tan")) x = Math.tan(Math.toRadians(x));
                    else throw new RuntimeException("Unknown function: " + func);
                } else {
                    throw new RuntimeException("Unexpected: " + (char) ch);
                }

                if (eat('^')) x = Math.pow(x, parseFactor()); // exponentiation

                return x;
            }
        }.parse();
    }
}
