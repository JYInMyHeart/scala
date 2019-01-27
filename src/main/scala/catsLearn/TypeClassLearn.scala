package catsLearn

import javafx.scene.paint.Color

import scala.collection.mutable

class TypeClassLearn {
  def combineAll[A] (list:List[A],A:Monoid[A]):A = list.foldRight(A.empty)(A.combine)

  def combineAll1[A <: Monoid[A]](list:List[A]):A = ???
}

abstract final case class Pair[A,B](first:A, second:B) extends Monoid[Pair[A,B]]{
  def empty(implicit eva:A <:< Monoid[A],evb:A <:< Monoid[B]):Pair[A,B] = ???
}

trait Monoid[A]{
  def empty:A
  def combine(x:A,y:A) :A
}


trait Pet[A]{
  def name(a:A):String
  def renamed(a:A,newName:String):A
}

case class Fish(name:String,age:Int)

 object Fish{
   implicit class RenameOps[A](a:A)(implicit ev:Pet[A]){
     def renamed(newName:String): A = ev.renamed(a,newName)
   }

   implicit val FishPet: Pet[Fish] = new Pet[Fish] {
     override def name(a:Fish): String = a.name

     override def renamed(a: Fish, newName: String): Fish = a.copy(name = newName)
   }
 }


//case class Kitty(name:String,color:Color) extends Pet[Kitty]{
//  override def renamed(newName: String): Kitty = Kitty(newName,Color.ALICEBLUE)
//}

trait Animal{
  def name:String
}
trait Rename[A]{
  def rename(a:A,newName:String):A
}

case class Dog(name:String,age:Int) extends Animal

object Dog{
  implicit class RenameOps[A](a:A)(implicit ev:Rename[A]){
    def renamed(newName:String): A = ev.rename(a,newName)
  }

  implicit val DogRename: Rename[Dog] = (a: Dog, newName: String) => a.copy(newName)
}





object Pet extends App {

//  def esquire[A <: Pet[A]](a:A):A = a.renamed(a.name + ", Esq")
//  val a1 = Fish("Jimmy",2)
//  val v = a1.renamed("Bob")
////  val a = Dog("J",4)
////  val b = a.renamed("s")
//  println(v)

  val q = mutable.Queue[P]()
  case class P(name:String,age:Int)
  val a = List(P("a",1),P("b",2),P("c",3),P("d",2))
  q ++= a
  val d = q.find(_.age == 2).get
  println(q)
  println(d)
}