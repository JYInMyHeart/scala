package latte

import java.util

case class LineCol(fileName: String,
                   line: Int,
                   column: Int)

object LineCol {
  val SYNTHETIC: LineCol = LineCol(null, 0, 0)
}

case class PairEntry(key:String,
                     startNode: ElementStartNode)

case class Args() {
  var fileName: String = _
  var previous: Node = _
  var currentLine: Int = _
  var currentCol: Int = _
  import util.Stack
  val startNodeStack: Stack[ElementStartNode] =
    new Stack[ElementStartNode]()
  val pairEntryStack:Stack[PairEntry] = new Stack[PairEntry]
  lazy val generateLineCol = LineCol(fileName,currentLine,currentCol)
  var defined:Map[String,String] = Map()
}

abstract class Node {
  var next: Node
  var previous: Node
  var lineCol: LineCol

  def this(args: Args) = {
    this
    lineCol = LineCol(args.fileName,args.currentLine,args.currentCol)
    this.previous = args.previous
    if(hasPrevious){
      assert(previous != null)
      previous.next = this
    }else if(!args.startNodeStack.isEmpty)
      args.startNodeStack.lastElement().linkNode = this
  }

  def hasNext:Boolean = next != null

  def hasPrevious: Boolean = previous != null

  def toString(indent:Int):String
}

case class ElementStartNode(args: Args,indent:Int) extends Node{
  var linkNode:Node = _


  override def equals(obj: Any): Boolean = {
    if(this == obj) return true
    if(obj == null || getClass != obj.getClass) return false
    val that:ElementStartNode = obj.asInstanceOf[ElementStartNode]
    linkNode == that.linkNode
  }

  override def toString: String = "NewLayer"

  def hasLinkedNode: Boolean = linkNode != null
  def toString(indent:Int):String = {
    val buffer:String = ""
    if(hasLinkedNode)
      buffer.concat(linkNode.toString(indent + 4))
    if(hasNext)
      buffer.concat(next.toString(indent))
    buffer
  }

  override var next: Node = _
  override var previous: Node = _
  override var lineCol: LineCol = _
}
