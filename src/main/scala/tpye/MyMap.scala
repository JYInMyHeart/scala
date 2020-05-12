package tpye

import scala.collection.mutable.ListBuffer

class MyMap[K,V] {

  class Node(val k: K){

    var v:V = null.asInstanceOf[V]

  }
  object Node{
    def newNode(k:K,v:V): Node ={
      val node = new Node(k)
      node.v = v
      node
    }
  }

  private val values = ListBuffer[Node]()

  def keyset: Set[K] = values.map(_.k).toSet

  def containsKey(k: K):Boolean = keyset.contains(k)
  def put(k: K,v:V):Unit = {
    if(containsKey(k)){
      values.filter(_.k == k).head.v = v
    }else{
      values += Node.newNode(k,v)
    }
  }

  def get[T <: V](k: K):T = {
    if(containsKey(k)){
       values.filter(_.k == k).head.v.asInstanceOf[T]
    }else{
       null.asInstanceOf[T]
    }
  }


}

object MyMap extends App {
  class A
  class B extends A
  var map = new MyMap[String,A]()
  var map2 = new MyMap[String,Int]()
  var map1 = new MyMap[String,B]()
//  map1 = map
//  map2.put("1",1)
//  map2.put("2",2)
//  map2.put("3",3)
//  println(map2.get("1"))
}
