package jawa;

public class Demo1 {

    public static void main(String[] args) {
        String a = new String("hello");
        changeStr(a);
        System.out.println(a);
    }
    static void changeStr(String str){
        str = "afaf";
        str = str + "sb";
    }
}
