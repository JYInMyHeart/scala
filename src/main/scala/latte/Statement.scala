package latte


trait Statement {
  def lineCol: LineCol
}

trait Expression extends Statement

case class Access(expression: Expression,
                  name: String,
                  lineCol: LineCol) extends Expression {
  override def hashCode(): Int = {
    val result = if (expression != null)
      expression.hashCode
    else
      0
    31 * result + (if (name == null) 0 else name.hashCode)
  }


  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    expression == obj.asInstanceOf[Access].expression
  }

  override def toString: String = s"($expression.$name)"
}

case class Anno(anno: Access,
                args: List[Assignment],
                lineCol: LineCol) extends Expression {

}

case class Assignment(assignTo: Access,
                      op: String,
                      assignFrom: Expression,
                      lineCol: LineCol) extends Expression {
  override def hashCode(): Int = {
    var result = assignTo.hashCode()
    result = 31 * result + op.hashCode
    result = 31 * result + assignFrom.hashCode()
    result
  }


  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case Assignment(at, o, af, l) =>
        at == assignTo && af == assignFrom && op == o
      case _ =>
        false
    }
  }

  override def toString: String = s"Assignment($assignTo $op $assignFrom)"
}

abstract class Literal(literalType: Int,
                       literal: String,
                       lineCol: LineCol) extends Expression {
  override def hashCode(): Int = {
    var result = literalType
    val h = if (literal == null) 0 else literal.hashCode
    result = 31 * result + h
    result
  }


  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    literal == obj.asInstanceOf[Literal].literal
  }

  override def toString: String = literal
}

object Literal {
  val NUMBER = 0
  val STRING = 1
  val BOOL = 2
}

case class NumberLiteral(literal: String,
                         lineCol: LineCol)
  extends Literal(Literal.NUMBER, literal, lineCol)

case class BoolLiteral(literal:String,
                       lineCol: LineCol)
  extends Literal(Literal.BOOL, literal, lineCol)

case class StringLiteral(literal:String,
                       lineCol: LineCol)
  extends Literal(Literal.STRING, literal, lineCol)

case class TypeOf(access: Access,
                  lineCol: LineCol) extends Expression{
  override def hashCode(): Int = access.hashCode()



  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    access == obj.asInstanceOf[TypeOf].access
  }

  override def toString: String = s"(type $access)"
}

case class Null(lineCol: LineCol) extends Expression{
  override def hashCode(): Int = 0

  override def equals(obj: Any): Boolean = obj.isInstanceOf[Null]

  override def toString: String = "(null)"
}


case class Invocation(access: Access,
                      args:List[Expression],
                      lineCol: LineCol) extends Expression{
  override def hashCode(): Int = {
    var result = if (access == null) 0 else access.hashCode
    result = 31 * result + args.hashCode()
    result
  }



  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    access == obj.asInstanceOf[Invocation].access
  }

  override def toString: String = {
    s"Invocation($access(${args.foldLeft("")(_+"."+_).substring(1)}))"
  }
}















trait Pre extends Statement

case class Modifier(modifier: String,
                    lineCol: LineCol) extends Pre {

  override def hashCode(): Int =
    if (modifier != null)
      modifier.hashCode
    else
      0

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    modifier == obj.asInstanceOf[Modifier].modifier
  }

  override def toString: String = s"($modifier)"
}
