public class ExceptionTest {
    public static void main(String[] args) {
        try {
            f();
        }catch (Exception e){
            System.out.println(e);
        }
    }

    public static void f() throws Exception{
        try{
            throw new NullPointerException("eeor");
        }finally {
            System.out.println("finally");
        }
    }
}
