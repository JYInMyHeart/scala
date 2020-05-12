package programming.in.scala

class TypeLevelTest {

  trait Monoid[A] {
    def empty: A
    def combine(x: A, y: A): A

  }

  val intAdditionMonoid: Monoid[Int] = new Monoid[Int] {
    override def empty: Int = 0

    override def combine(x: Int, y: Int): Int = x + y
  }
  def combineAll[A](list: List[A], a: Monoid[A]): A =
    list.foldRight(a.empty)(a.combine)
}
case class ItemName(value: String) extends AnyVal
case class Item(name: ItemName, price: BigDecimal)
trait ItemRepository[F[_]] {
  def findAll: F[List[Item]]
  def find(name: ItemName): F[Option[Item]]
  def save(item: Item): F[Unit]
  def remove(name: ItemName): F[Unit]
}

//F-bounded and self type
//trait Pet[A <: Pet[A]]{ this : A =>
//  def name:String
//  def rename(newName:String):A
//  def esquire[A <: Pet[A]](a: A): A = a.rename(a.name + ", Esq.")
//}
//
//case class Fish(name:String,age:Int) extends Pet[Fish]{
//  override def rename(newName:String):Fish = copy(name = newName)
//}
//
//case class Kitty(name:String) extends Pet[Kitty]{
//  override def rename(newName:String):Kitty = Kitty(newName)
//}

object app {
  import java.awt.Color

  trait Pet[A] {
    def name(a: A): String
    def renamed(a: A, newName: String): A
  }

  implicit class PetOps[A](a: A)(implicit ev: Pet[A]) {
    def name = ev.name(a)
    def renamed(newName: String): A = ev.renamed(a, newName)
  }

  case class Fish(name: String, age: Int)

  object Fish {
    implicit object FishPet extends Pet[Fish] {
      def name(a: Fish) = a.name
      def renamed(a: Fish, newName: String) = a.copy(name = newName)
    }
  }

  case class Kitty(name: String, color: Color)

  object Kitty {
    implicit object KittyPet extends Pet[Kitty] {
      def name(a: Kitty) = a.name
      def renamed(a: Kitty, newName: String) = a.copy(name = newName)
    }
  }

  trait ∃[F[_]] {
    type A
    val a: A
    val fa: F[A]
    override def toString = a.toString
  }

  object ∃ {
    def apply[F[_], A0](a0: A0)(implicit ev: F[A0]): ∃[F] =
      new ∃[F] {
        type A = A0
        val a = a0
        val fa = ev
      }
  }

  def main(args: Array[String]): Unit = {
    def esquire[A: Pet](a: A): A = a.renamed(a.name + ", Esq.")

    val bob = Fish("Bob", 12)
    val thor = Kitty("Thor", Color.ORANGE)
//    val pets = List[(A, Pet[A]) forSome { type A }]((bob, implicitly[Pet[Fish]]), (thor, implicitly[Pet[Kitty]]))
    println(List[∃[Pet]](∃(bob), ∃(thor)).map(e => ∃(esquire(e.a)(e.fa))(e.fa)))
//    println(pets.map { case (a, pa)  => esquire(a)(pa) })
//
//    println(0 ^ 4)
//    println(4 & 4)
//    println(0 & 4)

    val a: Int = 1
    val b: Number = 3

  }
}
