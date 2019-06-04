package jawa;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.function.BinaryOperator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Interpreter {
    public static void main(String[] args) {
        Interpreter interpreter = new Interpreter();
        System.out.println(interpreter.input("fn f a b => a + b "));
        System.out.println(interpreter.input("f 2 1"));
        System.out.println(interpreter.input("a = 2"));
        System.out.println(interpreter.input("f a a"));

    }

    private static final Pattern VALID_TOKENS = Pattern.compile("=>|[-+*/%=()]|[A-Za-z_][A-Za-z0-9_]*|[0-9]*(\\.?[0-9]+)");
    private static final Pattern NUMBER = Pattern.compile("[0-9]*(\\.?[0-9]+)");
    private static final Pattern VARIABLE = Pattern.compile("[A-Za-z_][A-Za-z0-9_]*");
    private static final Pattern ARITHMETIC_OPERATOR = Pattern.compile("[-+*/%]");

    private final Map<String, Double> variables = new HashMap<>();
    private final Map<String, Function> functions = new HashMap<>();

    public Double input(final String input) {
        return new LineInterpreter(variables, functions).interpret(tokenize(input));
    }

    private static class LineInterpreter {

        private final Map<String, Double> variables;
        private final Map<String, Function> functions;
        private final Stack<Node> output = new Stack<>();
        private final Stack<Operator> operators = new Stack<>();

        private LineInterpreter(final Map<String, Double> variables, final Map<String, Function> functions) {
            this.variables = variables;
            this.functions = functions;
        }

        private Double interpret(final Deque<String> tokens) {
            if ("fn".equals(tokens.peek())) {
                buildFunction(tokens);
            } else if (!tokens.isEmpty()) {
                return buildExpression(tokens, null).eval(variables);
            }
            return null;
        }

        private void buildFunction(final Deque<String> tokens) {
            tokens.pop();
            final String functionName = tokens.pop();
            if (variables.containsKey(functionName)) {
                throw new IllegalArgumentException("Identifier already taken by variable: " + functionName);
            }
            final List<String> functionParameterNames = new LinkedList<>();
            while (!"=>".equals(tokens.peek())) {
                final String functionParameterName = tokens.pop();
                if (functionParameterNames.contains(functionParameterName)) {
                    throw new IllegalArgumentException("Function parameter name already taken: " + functionParameterName);
                }
                functionParameterNames.add(functionParameterName);
            }
            tokens.pop();
            functions.put(functionName, new Function(functionName, functionParameterNames, buildExpression(tokens, functionParameterNames)));
        }

        private Node buildExpression(final Deque<String> tokens, final List<String> restrictedVariables) {
            while (!tokens.isEmpty()) {
                final String token = tokens.pop();
                if (isNumber(token)) {
                    output.push(new DoubleNode(token));
                } else if (isIdentifier(token)) {
                    final String next = tokens.peek();
                    if ("=".equals(next) && functions.containsKey(token)) {
                        throw new IllegalArgumentException("Identifier already taken by function: " + token);
                    }
                    if (functions.containsKey(token)) {
                        operators.add(new FunctionCallOperator(token));
                    } else {
                        if (restrictedVariables != null && !restrictedVariables.contains(token)) {
                            throw new IllegalArgumentException("Unknown variable: " + token);
                        }
                        output.push(new VariableNode(token));
                    }
                } else if ("(".equals(token)) {
                    operators.push(ArithmeticOperator.LPAR);
                } else if (")".equals(token)) {
                    while (operators.peek() != ArithmeticOperator.LPAR) {
                        addNode();
                    }
                    operators.pop();
                } else if (isArithmeticOperator(token)) {
                    final ArithmeticOperator operator = ArithmeticOperator.fromToken(token);
                    while (!operators.isEmpty() && operators.peek().getPrecedenceLevel() <= operator.precedenceLevel) {
                        addNode();
                    }
                    operators.push(operator);
                } else if ("=".equals(token)) {
                    operators.push(ArithmeticOperator.ASSIGN);
                }
            }
            while (!operators.isEmpty()) {
                addNode();
            }
            if (output.size() != 1) {
                throw new IllegalStateException("Output stack expected to contain only one element: " + output);
            }
            return output.pop();
        }

        private void addNode() {
            final Operator operator = operators.pop();
            if (operator instanceof FunctionCallOperator) {
                final FunctionCallOperator functionCallOp = (FunctionCallOperator) operator;
                final Function function = functions.get(functionCallOp.functionName);
                final List<String> functionParameterNames = new ArrayList<>(function.functionParameterNames);
                Collections.reverse(functionParameterNames);
                final Map<String, Node> functionArguments = new HashMap<>();
                for (final String functionParameterName : functionParameterNames) {
                    final Node argument = output.pop();
                    functionArguments.put(functionParameterName, argument);
                }
                output.push(new FunctionCallNode(function, functionArguments));
            } else {
                final Node operandB = output.pop();
                final Node operandA = output.pop();
                final Node newNode;
                if (operator == ArithmeticOperator.ASSIGN) {
                    final VariableNode variable = (VariableNode) operandA;
                    newNode = new AssignmentNode(variable, operandB);
                } else {
                    newNode = new ArithmeticOperatorNode((ArithmeticOperator) operator, operandA, operandB);
                }
                output.push(newNode);
            }
        }

        private static boolean isNumber(final String token) {
            return NUMBER.matcher(token).matches();
        }

        private static boolean isIdentifier(final String token) {
            return VARIABLE.matcher(token).matches();
        }

        private static boolean isArithmeticOperator(final String token) {
            return ARITHMETIC_OPERATOR.matcher(token).matches();
        }
    }

    private static Deque<String> tokenize(final String input) {
        final Deque<String> tokens = new LinkedList<>();
        final Matcher matcher = VALID_TOKENS.matcher(input);
        while (matcher.find()) {
            tokens.add(matcher.group());
        }
        return tokens;
    }

    private interface Node {
        Double eval(Map<String, Double> variables);
    }

    private static class ArithmeticOperatorNode implements Node {

        private final ArithmeticOperator operator;
        private final Node firstOperand;
        private final Node secondOperand;

        ArithmeticOperatorNode(final ArithmeticOperator operator, final Node firstOperand, final Node secondOperand) {
            this.operator = operator;
            this.firstOperand = firstOperand;
            this.secondOperand = secondOperand;
        }

        @Override
        public Double eval(final Map<String, Double> variables) {
            return operator.operation.apply(firstOperand.eval(variables), secondOperand.eval(variables));
        }
    }

    private interface Operator {
        int getPrecedenceLevel();
    }

    private enum ArithmeticOperator implements Operator {

        ADD("+", 4, (a, b) -> a + b),
        SUB("-", 4, (a, b) -> a - b),
        MUL("*", 3, (a, b) -> a * b),
        DIV("/", 3, (a, b) -> a / b),
        MOD("%", 3, (a, b) -> a % b),
        LPAR("(", 9, null),
        ASSIGN("=", 10, null);

        private final String token;
        private final int precedenceLevel;
        private final BinaryOperator<Double> operation;

        ArithmeticOperator(final String token, final int precedenceLevel, final BinaryOperator<Double> operation) {
            this.token = token;
            this.precedenceLevel = precedenceLevel;
            this.operation = operation;
        }

        private static ArithmeticOperator fromToken(final String token) {
            for (final ArithmeticOperator operator : values()) {
                if (operator.token.equals(token)) {
                    return operator;
                }
            }
            throw new IllegalArgumentException("No valid operator token: " + token);
        }

        @Override
        public int getPrecedenceLevel() {
            return precedenceLevel;
        }
    }

    private static class DoubleNode implements Node {

        private final Double value;

        DoubleNode(final String token) {
            value = Double.parseDouble(token);
        }

        @Override
        public Double eval(final Map<String, Double> variables) {
            return value;
        }
    }

    private static class VariableNode implements Node {

        private final String name;

        VariableNode(final String name) {
            this.name = name;
        }

        @Override
        public Double eval(final Map<String, Double> variables) {
            final Double value = variables.get(name);
            if (value == null) {
                throw new IllegalArgumentException("ERROR: Invalid identifier. No variable with name '" + name + "' was found.");
            }
            return value;
        }
    }

    private static class AssignmentNode implements Node {

        private final VariableNode variableNode;
        private final Node valueNode;

        private AssignmentNode(final VariableNode variableNode, final Node valueNode) {
            this.variableNode = variableNode;
            this.valueNode = valueNode;
        }

        @Override
        public Double eval(final Map<String, Double> variables) {
            final Double value = valueNode.eval(variables);
            variables.put(variableNode.name, value);
            return value;
        }
    }

    private static class FunctionCallNode implements Node {

        private final Function function;
        private final Map<String, Node> functionArguments;

        private FunctionCallNode(final Function function, final Map<String, Node> functionArguments) {
            this.function = function;
            this.functionArguments = functionArguments;
        }

        @Override
        public Double eval(final Map<String, Double> variables) {
            final Map<String, Double> argumentsAndVariable = new HashMap<>(variables);
            for (final Map.Entry<String, Node> argument : functionArguments.entrySet()) {
                final Double value = argument.getValue().eval(variables);
                argumentsAndVariable.put(argument.getKey(), value);
            }
            return function.node.eval(argumentsAndVariable);
        }
    }

    private static class Function {

        private final String functionName;
        private final List<String> functionParameterNames;
        private final Node node;

        private Function(final String functionName, final List<String> functionParameterNames, final Node node) {
            this.functionName = functionName;
            this.functionParameterNames = functionParameterNames;
            this.node = node;
        }
    }

    private static class FunctionCallOperator implements Operator {

        private final String functionName;

        private FunctionCallOperator(final String functionName) {
            this.functionName = functionName;
        }

        @Override
        public int getPrecedenceLevel() {
            return 11;
        }
    }
}