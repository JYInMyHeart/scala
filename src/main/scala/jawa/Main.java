package jawa;
import java.util.ArrayList;
import java.util.List;
public class Main {
    int a = 0;
    public static void main(String[] args) throws InterruptedException {
       Main m = new Main();
       List<Thread> list = new ArrayList<>();
       for(int i = 0;i < 5;i++){
           list.add(new Thread(() -> {
               int j = 0;
               while(j < 1000000){
                   m.a++;
                   System.out.println(m.a);
                   j++;
               }

           }));
       }
       list.forEach(Thread::start);
       Thread.sleep(1000);
        System.out.println(m.a);
    }


}


