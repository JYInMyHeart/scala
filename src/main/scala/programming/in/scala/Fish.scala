package programming.in.scala

//trait Pet {
//  def name: String
//}
//
//trait Rename[B] {
//  def rename(a: B, newName: String): B
//}
//
//case class Fish(name: String, age: Int) extends Pet
//
//object Fish {
//  implicit val FishRename = new Rename[Fish] {
//    override def rename(a: Fish, newName: String): Fish = a.copy(name = newName)
//  }
//
//  implicit class RenameOps[A](a: A)(implicit ev: Rename[A]) {
//    def renamed(newName: String): A = ev.rename(a, newName)
//  }
//}
