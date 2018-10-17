package code;



import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

public class MathEvaluator {
    Stack<Op> ops = new Stack<>();
    Stack<Token> vals = new Stack<>();

    public static void main(String[] args) {
        System.out.println(new MathEvaluator().calculate("1231+(-232)"));
    }

    public double calculate(String expression) {
        List<String> list = lexer(expression);
        for(String i:list){
            pushIn(i);
        }
        while(!ops.empty()){
            Op op = ops.pop();
            Token t1 = popleft();
            Token t2 = popleft();
            eval(op, t2, t1);
        }
        return Double.parseDouble(vals.pop().left);
    }

    enum Op {
        Plus,
        Mul,
        Sub,
        Div
    }

    public int getPriority(Op op) {
        switch (op) {
            case Plus:
                return 1;
            case Mul:
                return 2;
            case Sub:
                return 1;
            case Div:
                return 2;
        }
        return 0;
    }


    public void eval(Op op, Token t1, Token t2) {
        switch (op) {
            case Plus:
                vals.push(t1.add(t2));break;
            case Mul:
                vals.push(t1.mul(t2));break;
            case Sub:
                vals.push(t1.sub(t2));break;
            case Div:
                vals.push(t1.div(t2));break;
        }
    }

    class Token {
        String left;
        String right;

        public Token(String left) {
            this.left = left;
        }

        public Token(String left, String right) {
            this.left = left;
            this.right = right;
        }

        public double getNum(String v) {
            return Double.valueOf(v);
        }

        public Token add(Token t) {
            return new Token(String.valueOf(getNum(t.left) + getNum(left)));
        }

        public Token mul(Token t) {
            return new Token(String.valueOf(getNum(t.left) * getNum(left)));
        }

        public Token sub(Token t) {
            return new Token(String.valueOf(getNum(t.left) - getNum(left)));
        }

        public Token div(Token t) {
            return new Token(String.valueOf(getNum(t.left) / getNum(left)));
        }
    }

    public Token popleft() {
        return vals.pop();
    }

    public Token popright() {
        return vals.pop();
    }

    public void pushIn(String s) {
        if ("+".equals(s))
            ops.push(Op.Plus);
        if ("-".equals(s))
            ops.push(Op.Sub);
        if ("*".equals(s))
            ops.push(Op.Mul);
        if ("/".equals(s))
            ops.push(Op.Div);
        if ("(".equals(s))
            vals.push(new Token(null, s));
        if ((s.startsWith("-") && s.substring(1).matches("[0-9]*")) || s.matches("[0-9]*"))
            vals.push(new Token(s));
        if (")".equals(s)) {
            Op op = ops.pop();
            Token t1 = popleft();
            Token t2 = popleft();
            eval(op, t2, t1);
        }
    }

    public List<String> lexer(String expression) {
        List<String> res = new ArrayList<>();
        char[] chars = expression.toCharArray();
        int i = 0;
        while(i < chars.length){
            char value = chars[i];
            if(' ' == value){
                i++;
                continue;
            }

            if(value == '('){
                i++;
                res.add("(");
            }
            if(String.valueOf(value).matches("[0-9]")){
                i = getI(res, chars, i,true);
            }
            if(value == '-'){
                if(String.valueOf(chars[i + 1]).matches("[0-9]")){
                    i++;
                    i = getI(res, chars, i,false);
                }else{
                    i++;
                    res.add("-");
                }
            }
            if(value == '+'){
                i++;
                res.add("+");
            }
            if(value == '*'){
                i++;
                res.add("*");
            }
            if(value == '/'){
                i++;
                res.add("/");
            }
        }
        return res;
    }

    private int getI(List<String> res, char[] chars, int i,boolean sign) {
        int t = i;
        StringBuilder tempStr = new StringBuilder();
        if(!sign) tempStr.append("-");
        while(t < chars.length && String.valueOf(chars[t]).matches("[0-9]")){
            tempStr.append(chars[t++]);
        }
        res.add(tempStr.toString());
        return t;
    }


}
