package jawa;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class Interpreter3 {

    class Variable {
        Double value=null;

        @Override
        public String toString() {
            return "Variable{" +
                     value +
                    '}';
        }
    }

    private Map<String, Variable> variables = new HashMap<>();

    interface ASTNode {
        double eval();
    }

    class LiteralNode implements ASTNode {
        private Double value;

        public LiteralNode(Double value) {
            this.value = value;
        }

        @Override
        public double eval() {
            return value;
        }

        @Override
        public String toString() {
            return "LiteralNode{" +
                     value +
                    '}';
        }
    }

    class VariableNode implements ASTNode {
        private Variable variable;

        public VariableNode(Variable variable) {
            this.variable = variable;
        }

        @Override
        public double eval() {
            if (variable.value==null)
                throw new IllegalArgumentException("ERROR: Variable not initialized");
            return variable.value;
        }

        @Override
        public String toString() {
            return "VariableNode{" +
                     variable +
                    '}';
        }
    }

    class UnaryNode implements ASTNode {
        private ASTNode subNode;
        private String op;

        public UnaryNode(String op, ASTNode subNode) {
            this.subNode = subNode;
            this.op = op;
        }

        @Override
        public double eval() {
            if (op.equals("-"))
                return -subNode.eval();
            else
                return subNode.eval();
        }

        @Override
        public String toString() {
            return "UnaryNode{" +
                     subNode +
                    ", '" + op + '\'' +
                    '}';
        }
    }

    class BinaryNode implements ASTNode {
        private ASTNode left;
        private ASTNode right;
        private String op;

        public BinaryNode(ASTNode left, ASTNode right, String op) {
            this.left = left;
            this.right = right;
            this.op = op;
        }

        @Override
        public double eval() {
            double l = left.eval();
            double r = right.eval();
            switch (op) {
                case "+":
                    return l + r;
                case "-":
                    return l - r;
                case "*":
                    return l * r;
                case "/":
                    return l / r;
                case "%":
                    return l % r;
            }
            throw new IllegalArgumentException("ERROR: wrong binary operator '" + op + "'");
        }

        @Override
        public String toString() {
            return "BinaryNode{" +
                     left +
                    ", " + right +
                    ", '" + op + '\'' +
                    '}';
        }
    }

    class AssignmentNode implements ASTNode {
        private VariableNode left;
        private ASTNode right;

        public AssignmentNode(VariableNode left, ASTNode right) {
            this.left = left;
            this.right = right;
        }

        @Override
        public double eval() {
            return left.variable.value = right.eval();
        }

        @Override
        public String toString() {
            return "AssignmentNode{" +
                     left +
                    ", " + right +
                    '}';
        }
    }

    class Parser {

        private Deque<String> tokens;

        public Parser(Deque<String> tokens) {
            this.tokens = tokens;
        }

        private void skip(String token) {
            String s = tokens.poll();
            if (null == s || !s.equals(token))
                throw new IllegalArgumentException("ERROR: '" + token + "' expected");
        }

        private ASTNode parseFactor() {
            String token = tokens.poll();
            if (token == null)
                throw new IllegalArgumentException("ERROR: factor expected");
            switch (token) {
                case "(": {
                    ASTNode result = parseExpression();
                    skip(")");
                    return result;
                }
                case "+":
                case "-": {
                    return new UnaryNode(token, parseFactor());
                }
                default: {
                    char c = token.charAt(0);
                    if (c >= '0' && c <= '9') {
                        try {
                            return new LiteralNode(Double.parseDouble(token));
                        } catch (NumberFormatException ignored) {
                            throw new IllegalArgumentException("ERROR: wrong number " + token);
                        }
                    }
                    if (c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z') {
                        return new VariableNode(variables.computeIfAbsent(token, (name) -> new Variable()));
                    }
                    throw new IllegalArgumentException("ERROR: factor expected, but found " + token);
                }
            }
        }

        private ASTNode parseMul() {
            ASTNode result = parseFactor();
            while (true) {
                String next = tokens.peek();
                if (next == null || !next.equals("*") && !next.equals("/") && !next.equals("%"))
                    return result;
                tokens.poll();
                ASTNode right = parseFactor();
                result = new BinaryNode(result, right, next);
            }
        }

        private ASTNode parseSum() {
            ASTNode result = parseMul();
            while (true) {
                String next = tokens.peek();
                if (next == null || !next.equals("+") && !next.equals("-"))
                    return result;
                tokens.poll();
                ASTNode right = parseMul();
                result = new BinaryNode(result, right, next);
            }
        }

        private ASTNode parseExpression() {
            ASTNode result = parseSum();
            String next = tokens.peek();
            if (next == null || !next.equals("="))
                return result;
            tokens.poll();
            if (!(result instanceof VariableNode))
                throw new IllegalArgumentException("ERROR:Left side of assignment must be identifier");
            ASTNode right = parseExpression();
            result = new AssignmentNode((VariableNode) result, right);
            return result;
        }
    }

    public Double input(String input) {
        Deque<String> tokens = tokenize(input);
        if (tokens.isEmpty()) return null;
        ASTNode node = new Parser(tokens).parseExpression();
        System.out.println(node);
        if (!tokens.isEmpty())
            throw new IllegalArgumentException("ERROR: Unexpected tokens after expression: "+tokens.stream().collect(Collectors.joining(" ")));
        return node.eval();
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

    public static void main(String[] args) {
        Interpreter3 in3 = new Interpreter3();
        System.out.println(in3.input("1+(x=y=3)"));
    }
}
