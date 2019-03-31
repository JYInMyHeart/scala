public class Main4 {
    int count = 1;
    public static void main(String[] args) throws InterruptedException {
        Main4 m = new Main4();
        Worker w1 = new Worker(m);
        Worker w2 = new Worker(m);
        Worker w3 = new Worker(m);
        Worker w4 = new Worker(m);
        new Thread(w1).start();
        new Thread(w2).start();
        new Thread(w3).start();
        new Thread(w4).start();

    }
    void print() {
        synchronized (this) {
            System.out.println(Thread.currentThread().getName() + ": " + count);
            count++;
            this.notifyAll();
        }
    }
    static class Worker implements Runnable {
        final Main4 m;

        public Worker(Main4 m) {
            this.m = m;
        }

        @Override
        public void run() {

            synchronized (m) {
                while (m.count <= 20) {
                    try {
                        m.print();
                        m.wait();
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }
        }
    }
}


interface Nat {
}

class Z implements Nat {
}

class Succ<T extends Nat> implements Nat {
}

interface Lt<A extends Nat, B extends Nat> {

}



