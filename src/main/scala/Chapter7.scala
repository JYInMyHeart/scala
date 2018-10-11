import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}



class Chapter7 {

}



object Par {

  type Par[A] = ExecutorService => Future[A]
  def unit[A](a: => A): Par[A] = (es:ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get:A) extends Future[A]{
    def isDone = true
    def get(timeout:Long,units:TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning:Boolean):Boolean = false
  }

  def run[A](s:ExecutorService)(a: Par[A]): Future[A] = a(s)

  def map2[A,B,C](time:Long)(p1:Par[A],p2:Par[B])(f:(A,B) => C):Par[C] =
    (es:ExecutorService) =>{
      val af = p1(es)
      val bf = p2(es)
      UnitFuture(f(af.get(time,TimeUnit.NANOSECONDS),bf.get(time,TimeUnit.NANOSECONDS)))
    }


  def fork[A](a: => Par[A]):Par[A] =
    es => es.submit(new Callable[A]{
      def call = a(es).get
    })

  def lazyUnit[A](a : => A):Par[A] = fork(unit(a))

  def asyncF[A,B](f:A => B):A => Par[B] =
    a =>
      lazyUnit(f(a))

  def map[A,B](pa:Par[A])(f:A => B):Par[B] =
    map2(10)(pa,unit(()))((a,_) => f(a))

  def sum(time:Long)(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.size / 2)
      Par.map2(time)(Par.fork(sum(time)(l)), Par.fork(sum(time)(r)))(_ + _)
    }

  def sequence_simple[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(10)(h,t)(_ :: _))

  def sequenceRight[A](ps:List[Par[A]]):Par[List[A]] = ps match {
    case Nil => unit(Nil)
    case h :: t => map2(10)(h, fork(sequenceRight(t)))(_ :: _)
  }

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2(10)(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def parMap[A,B](ps:List[A])(f:A => B):Par[List[B]] = {
    val fbs:List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sortPar(parList:Par[List[Int]]):Par[List[Int]] =
    map(parList)(_.sorted)
}

