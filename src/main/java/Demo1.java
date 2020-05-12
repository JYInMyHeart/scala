/**
 * @Author: xzck
 * @File: Demo1
 * @Time: 11:35 2019/7/25
 */
public class Demo1 {

    public static void main(String[] args) {
        Exp exp = new Literal(10);
        Exp exp1 = new Add(exp,exp);
        ExpEvalVistor expEvalVistor = new ExpEvalVistor();
        System.out.println(exp1.accept(expEvalVistor));
        Eval eval = new Eval();
        Show show = new Show();
        System.out.println(eval.add(eval.literal(1), eval.literal(2)));
        System.out.println(show.add(show.literal(1), show.literal(2)));
    }

}

abstract class Nat1{}


class Zero1 extends Nat1{

}

class Succ1 extends Nat1{
    Nat1 predecessor;

    public Succ1(Nat1 predecessor) {
        this.predecessor = predecessor;
    }
}


//class Exp{
//    public int literal(int v){return v;}
//    public int add(int a,int b) {return a + b;}
//    public int subtract(int a,int b){return a - b;}
//    public int multipy(int a,int b){return a * b;}
//
//}

abstract class Exp{
    abstract <T> T accept(ExpVisitor<T> visitor);
}

class Literal extends Exp{
    public final int val;

    public Literal(int  val) {
        this.val = val;
    }


    @Override
    <T> T accept(ExpVisitor<T> visitor) {
        return visitor.forLiteral(this);
    }
}





class Add extends Exp{
    public final Exp v1;
    public final Exp v2;

    public Add(Exp v1, Exp v2) {
        this.v1 = v1;
        this.v2 = v2;
    }

    @Override
    <T> T accept(ExpVisitor<T> visitor) {
        return visitor.forAdd(this);
    }
}
class Subtract extends Exp{
    public final Exp v1;
    public final Exp v2;

    public Subtract(Exp v1, Exp v2) {
        this.v1 = v1;
        this.v2 = v2;
    }
    @Override
    <T> T accept(ExpVisitor<T> visitor) {
        return visitor.forSubtract(this);
    }
}

class Multiply extends Exp{
    public final Exp v1;
    public final Exp v2;

    public Multiply(Exp v1, Exp v2) {
        this.v1 = v1;
        this.v2 = v2;
    }

    @Override
    <T> T accept(ExpVisitor<T> visitor) {
        return visitor.forMultiply(this);
    }
}
class ExpEvalVistor implements ExpVisitor<Integer>{

    @Override
    public Integer forLiteral(Literal e) {
        return e.val;
    }

    @Override
    public Integer forAdd(Add e) {
        return  e.v1.accept(this) + e.v2.accept(this);
    }

    @Override
    public Integer forSubtract(Subtract e) {
        return (e.v1.accept(this)) - (e.v2.accept(this));
    }

    @Override
    public Integer forMultiply(Multiply e) {
        return (e.v1.accept(this)) * (e.v2.accept(this));
    }
}

class ExpStringVisitor implements ExpVisitor<String>{

    @Override
    public String forLiteral(Literal e) {
        return String.valueOf(e.val);
    }

    @Override
    public String forAdd(Add e) {
        return e.v1.accept(this) + " + " + e.v2.accept(this);
    }

    @Override
    public String forSubtract(Subtract e) {
        return e.v1.accept(this) + " - " + e.v2.accept(this);
    }

    @Override
    public String forMultiply(Multiply e) {
        return e.v1.accept(this) + " * " + e.v2.accept(this);
    }
}
//class DExp extends Exp{
//    public int divide(int a, int b){
//        return a / b;
//    }
//}

 interface ExpVisitor<T> {
    T forLiteral(Literal e);
    T forAdd(Add e);
    T forSubtract(Subtract e);
    T forMultiply(Multiply e);
}

interface FinalExp<T>{
    T literal(int v);
    T add(T v1, T v2);
    T subtract(T v1, T v2);
    T multiply(T v1, T v2);
}

class Eval implements FinalExp<Integer>{

    @Override
    public Integer literal(int v) {
        return v;
    }

    @Override
    public Integer add(Integer v1, Integer v2) {
        return v1 + v2;
    }

    @Override
    public Integer subtract(Integer v1, Integer v2) {
        return v1 - v2;
    }

    @Override
    public Integer multiply(Integer v1, Integer v2) {
        return v1 * v2;
    }
}

class Show implements FinalExp<String>{

    @Override
    public String literal(int v) {
        return String.valueOf(v);
    }

    @Override
    public String add(String v1, String v2) {
        return v1 + " + " + v2;
    }

    @Override
    public String subtract(String v1, String v2) {
        return v1 + " - " + v2;
    }

    @Override
    public String multiply(String v1, String v2) {
        return v1 + " * " + v2;
    }
}



class ExpShow{
    public String literal(int v) { return v + ""; }
    public String add(String a, String b) { return "(" + a + "+" + b + ")";}
    public String subtract(String a, String b) { return "(" + a + "-" + b + ")";}
    public String multiply(String a, String b) { return "(" + a + "*" + b + ")";}
}

interface Channel<T>{
    Channel<T> accept(ChannelVisitor<T> channelVisitor);
}

interface ChannelVisitor<T>{
    Channel<T> play(Channel<T> channel);
}

final class TestChannel<T> implements Channel<T>{

    @Override
    public Channel<T> accept(ChannelVisitor<T> channelVisitor) {
       return channelVisitor.play(this);
    }
}