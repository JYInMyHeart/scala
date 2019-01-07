package code;

import java.math.BigInteger;

public class Faberge {
    public static BigInteger h(BigInteger n,BigInteger m){
        BigInteger h = BigInteger.ZERO;
        BigInteger t = new BigInteger("1");
        for(long i = 1;i <= n.longValue();i++){
            t = t.multiply(m.add(BigInteger.valueOf(-i + 1)) ).divide(BigInteger.valueOf(i));
            h = h.add(t);
        }
        return h;
    }
}
