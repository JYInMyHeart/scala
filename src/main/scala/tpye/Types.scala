package tpye

/**
  * @author xck
  */
class XckType
class XckInt(var value:Int) extends XckType {
  override def toString: String = s"$value => {MalInt}"
}
class XckSymbol(var value:String) extends XckType{
  override def toString: String = s"$value => {MalSymbol}"
}
class XckList(var list:Array[XckType]) extends XckType{
  def getList(x:XckType*) : Array[XckType] = {
    list = x.toArray
    list
  }
  def isList : Boolean = true
  def size : Int = list.length
  def nth(index:Int) : XckType = list(index)
  override def toString: String = s"$list => {MalList}"
  def slice(start:Int,end:Int) : XckList = new XckList(list.toList.slice(start,end).toArray)
  def slice(index:Int) : XckList = slice(index,size)
}

class XckVector( var list1:Array[XckType]) extends XckList(list1){
  override def isList: Boolean = false
}

class XckConstant(var value:String) extends XckType{
  override def toString: String = s"$value => {MalConstant}"
}



