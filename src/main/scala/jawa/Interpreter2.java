package jawa;

import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Interpreter2 {
    private  Map<String, Double> map = new HashMap<>();

    public static void main(String[] args) {
        Interpreter2 in = new Interpreter2();
        Interpreter2 in1 = new Interpreter2();
        Interpreter2 in2 = new Interpreter2();
        System.out.println(in.input("x=7"));
        System.out.println(in1.input("x"));
        System.out.println(in2.input("x+3"));
        System.out.println(in2.input("y=x+5"));
        System.out.println(in2.input("1 2"));

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

    public Double input(String input) {
        Deque<String> tokens = tokenize(input);
        tokens.add("$");
        double x;
        try{
            x = new Parser(tokens).parse();
        }catch (Error e){
            x = 0;
        }
        return x;

    }
    class Parser {

        private Deque<String> tokens;
        private int pos;
        private String c;
        private int argOffset;

        public Parser(Deque<String> tokens){
            this.tokens = tokens;
            pos = 0;
            argOffset = 0;
        }

        double parse(){
            next();
            return parseExpression();
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


        public double parseFunc() {
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

        double parseExpr(){
          double x = parseExpression();
          if(eat("=")){
              double rhs = parseExpression();
              return rhs;
          }
          return x;
        }
        double parseExpression(){
            double x = parseTerm();
            while (true) {
                if (eat("+")) {
                    double rhs = parseTerm();
                    x = x + rhs;
                } else if (eat("-")) {
                    double rhs = parseTerm();
                    x = x - rhs;
                } else return x;
            }
        }

        double parseTerm() {
            double x = parseFactor();
            while (true) {
                if (eat("*")) {
                    double rhs = parseFactor();
                    x = rhs * x;
                } else if (eat("/")) {
                    double rhs = parseFactor();
                    x = x / rhs;
                } else if(eat("%")){
                    double rhs = parseFactor();
                    x = x % rhs;
                }else return x;
            }
        }

        double parseFactor() {
            if (c.matches("\\d+")) {
                String v = c;
                next();
                return Double.valueOf(v);
            } else if (c.matches("[A-Za-z_][A-Za-z0-9_]*")) {
                List<String> params = new ArrayList<>();
                while (true) {
                    if (c.matches("[A-Za-z_][A-Za-z0-9_]*") || (c.matches("\\d+"))) {
                        params.add(c);
                        next();
                        if (c.equals("=")) {
                            next();
                        }
                    } else
                        break;
                }
                String temp = params.get(params.size() - 1);
                double val;
                if (temp.matches("\\d+"))
                    val = Double.valueOf(temp);
                else{
                    try{
                        val = map.get(temp);
                    }catch (Exception e){
                        throw new Error("unknown");
                    }
                }

                for (int i = 0; i < params.size() - 1; i++) {
                    map.put(params.get(i), val);
                }
                return val;
            } else if(eat("(")){
                double expr = parseExpr();
                eat(")");
                return expr;
            }else{
                throw new Error("unknown characters");
            }
        }
    }

}