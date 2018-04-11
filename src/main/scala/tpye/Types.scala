package tpye

/**
  * @author xck
  */
sealed trait XckType
case class XckInt(var value:Int) extends XckType {
  override def toString: String = s"$value => {MalInt}"
}
case class XckSymbol(var value:String) extends XckType{
  override def toString: String = s"$value => {MalSymbol}"
}


