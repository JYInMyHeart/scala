/**
  * @author xck
  */
object Demo {
  class TypeA {}
  class TypeB extends TypeA {}
  var a = Map[String, TypeB]("1" -> new TypeB)
  var b = Map[String, T forSome { type T <: TypeA }]()
  b = a

}

class TEst {
  sealed trait Al
  case object A extends Al
  case object B extends Al
  case object C extends Al
  case object D extends Al
  def demoTest(a: Int): Al = {
    a match {
      case 1 => A
      case 2 => B
      case 3 => C
      case _ => D
    }
  }
}

abstract class Q[+A, +B](x: A, y: B) {
  val fst: A = x // **** error: illegal variance:
  val snd: B = y

}

class Animal {}
class Cat extends Animal {}
class Dog extends Animal {}
class Pets[+A](val p: A) {}
object Pets extends App {}
trait Bullet
class NormalBullet extends Bullet
class ExplosiveBullet extends Bullet

final class AmmoMagazine[+A <: Bullet](private[this] var bullets: List[A]) {

  def hasBullets: Boolean = bullets.nonEmpty

  def giveNextBullet(): Option[A] =
    bullets match {
      case Nil => {
        None
      }
      case t :: ts => {
        bullets = ts
        Some(t)
      }
    }

  private[this] def add(a: A): Unit = {
    bullets :+= a
  }
}

class VendingMachine[+A](val currentItem: Option[A], items: List[A]) {

  def this(items: List[A]) = this(None, items)

  def dispenseNext(): VendingMachine[A] =
    items match {
      case Nil => {
        if (currentItem != null)
          new VendingMachine(None, Nil)
        else
          this
      }
      case t :: ts => {
        new VendingMachine(Some(t), ts)
      }
    }

  def addAll[B >: A](newItems: List[B]): VendingMachine[B] =
    new VendingMachine(items ++ newItems)

}

class GarbageCan[-A] {
  val it: List[T forSome { type T >: A }] = List()
  private[this] var items: List[A] = List.empty
  def put(item: A): Unit = this.items :+= item
  def putAll(items: List[A]): Unit = this.items ++= items
  def itemsCount: Int = this.items.length
}
class Item {}
class PlasticItem extends Item {}
class PlasticBottle extends PlasticItem {
  def setGarbageCanForPlastic(gc: GarbageCan[PlasticItem]): Unit = {}

  setGarbageCanForPlastic(new GarbageCan[Item])
}

trait MusicInstrument {
  val productionYear: Int
}

case class Guitar(productionYear: Int) extends MusicInstrument
case class Piano(productionYear: Int) extends MusicInstrument

object Test1213 extends App {
  val a: MusicInstrument => Boolean = _.productionYear > 19

  val guitars = List[Guitar](Guitar(20), Guitar(30), Guitar(18))
  val pianos = List[Piano](Piano(17), Piano(30))

  println(guitars.filter(a))
  println(pianos.filter(a))
}

trait Observable[+T] {
  def subscribe(observer: Observer[T]): String
  def map[R](func: T => R): Observable[R]
}
trait Observer[-T] {
  def obNext(value: T): Unit = {}
}

class Box[A]() {

  private var thing: A = _

  def retrieve: A = thing

  def put(thing: A) = this.thing = thing

}
