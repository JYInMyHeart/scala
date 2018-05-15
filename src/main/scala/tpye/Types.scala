package tpye

/**
  * @author xck
  */
object Types {


  class XckType

  class XckInt(var value: Int) extends XckType {
    override def toString: String = s"$value => {XckInt}"
  }

  class XckSymbol(var value: String) extends XckType {
    override def toString: String = s"$value => {XckSymbol}"
  }

  class XckList(var list: Seq[XckType]) extends XckType {
    def getList(x: Seq[XckType]): Seq[XckType] = {
      list = x
      list
    }

    def list(x: XckType*): XckList = new XckList(getList(x))

    def isList: Boolean = true

    def size: Int = list.length

    def nth(index: Int): XckType = list(index)

    override def toString: String = {
      list.foreach(print(_)) + " => {XckList}"
    }

    def slice(start: Int, end: Int): XckList = new XckList(list.slice(start, end))

    def slice(index: Int): XckList = slice(index, size)
  }

  class XckVector(var list1: List[XckType]) extends XckList(list1) {
    override def isList: Boolean = false
  }

  class XckConstant(var value: String) extends XckType {
    override def toString: String = s"$value => {XckConstant}"
  }

  class XckBoolean(var value: Boolean) extends XckType {
    override def toString: String = s"$value => {XckBoolean}"
  }

  trait ILambda {
    def apply(a: XckList): XckType
  }

  abstract class XckFunc(var ast: XckType, var env: Environment, var params: XckList) extends XckType with ILambda {
    def getEnvironment(list: XckList): Environment = new Environment(null, null).getEnvironment(env, params, list)
  }

}
