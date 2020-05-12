import java.util.HashSet;
import java.util.Set;

/**
 * @Author: xck
 * @File: Test15
 * @Time: 14:02 2019/12/10
 */
public class Test15 {
    public static int max(int a, int b) {
        int[] t = new int[2];
        t[((a - b >> 31)) + 1] = a;
        t[((b - a >> 31) )+ 1] = b;
        return t[1];
    }

//    int compare(int a, int b) {
//        int diff = a ^ b;
//        if (diff == 0) return 0;
//
//        // 001xxxxx -> 00100000
//        diff |= diff >> 1;
//        diff |= diff >> 2;
//        diff |= diff >> 4;
//        diff |= diff >> 8;
//        diff |= diff >> 16;
//        diff ^= diff >> 1;
//
//        return (a & diff == 0) ? 1 : -1;
//    }



    public static void main(String[] args) {
//        System.out.println(max(2, 3));
//        System.out.println(max(Integer.MAX_VALUE - 1,Integer.MIN_VALUE + 1));

        System.out.println("\uFF1A");
        System.out.println("2：1".replace("：",":"));
        System.out.println("2：1".replace("\uFF1A",":"));
    }
}
