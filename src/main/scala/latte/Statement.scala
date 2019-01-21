package latte


trait Statement {
  def lineCol: LineCol
}

/**
  * Expression
  *
  */
trait Expression extends Statement


/**
  * Access
  *
  */
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


/**
  * Annotation
  *
  */
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


/**
  * Literal
  *
  */
abstract class Literal(val literalType: Int,
                       val literal: String,
                       val lineCol: LineCol) extends Expression {
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


/**
  * numberLiteral
  *
  */
case class NumberLiteral(override val literal: String,
                         override val lineCol: LineCol)
  extends Literal(Literal.NUMBER, literal, lineCol)

/**
  * boolLiteral
  *
  */
case class BoolLiteral(override val literal: String,
                       override val lineCol: LineCol)
  extends Literal(Literal.BOOL, literal, lineCol)


/**
  * stringLiteral
  *
  */
case class StringLiteral(override val literal: String,
                         override val lineCol: LineCol)
  extends Literal(Literal.STRING, literal, lineCol)


/**
  * type
  *
  */
case class TypeOf(access: Access,
                  lineCol: LineCol) extends Expression {
  override def hashCode(): Int = access.hashCode()


  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    access == obj.asInstanceOf[TypeOf].access
  }

  override def toString: String = s"(type $access)"
}


/**
  * null
  *
  */
case class Null(lineCol: LineCol) extends Expression {
  override def hashCode(): Int = 0

  override def equals(obj: Any): Boolean = obj.isInstanceOf[Null]

  override def toString: String = "(null)"
}


/**
  * invocation
  *
  */
case class Invocation(access: Access,
                      args: List[Expression],
                      lineCol: LineCol) extends Expression {
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
    s"Invocation($access(${args.foldLeft("")(_ + "." + _).substring(1)}))"
  }
}


/**
  * procedure
  *
  */
case class Procedure(statements: List[Statement],
                     lineCol: LineCol) extends Expression {
  override def hashCode(): Int =
    if (statements != null)
      statements.hashCode
    else
      0

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    statements == obj.asInstanceOf[Procedure].statements
  }

  override def toString: String = s"($statements)"
}

/**
  * undefined
  *
  */
case class UndefinedExp(lineCol: LineCol) extends Expression {
  override def hashCode(): Int = 0

  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[UndefinedExp]
  }

}

/**
  * as expression
  *
  */
case class AsType(exp: Expression,
                  access: Access,
                  lineCol: LineCol) extends Expression {
  override def hashCode(): Int = {
    var result = if (access == null) 0 else access.hashCode
    result = 31 * result + exp.hashCode()
    result
  }


  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    exp == obj.asInstanceOf[AsType].exp && access == obj.asInstanceOf[AsType].access
  }

  override def toString: String = s"return ($exp as $access)"
}


/**
  * index
  *
  */
case class Index(exp: Expression,
                 args: List[Expression],
                 lineCol: LineCol) extends Expression {
  override def hashCode(): Int = {
    var result = if (exp == null) 0 else exp.hashCode
    result = 31 * result + args.hashCode()
    result
  }


  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    exp == obj.asInstanceOf[Index].exp && args == obj.asInstanceOf[Index].args
  }

  override def toString: String =
    s"($exp[${args.foldLeft("")(_ + "." + _).substring(1)}])"
}


case class PackageRef(pkg: String,
                      lineCol: LineCol) extends Expression {
  override def hashCode(): Int =
    if (pkg != null)
      pkg.hashCode
    else
      0

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    pkg == obj.asInstanceOf[PackageRef].pkg
  }

  override def toString: String = s"($pkg)"
}


case class MapExp(map:Map[Expression,Expression],
                  lineCol:LineCol) extends Expression{
  
}

case class ArrayExp(list:List[Expression],
                  lineCol:LineCol) extends Expression{

}


trait Pre extends Statement

/**
  * modifier
  *
  */
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


/**
  * return
  *
  */
case class Return(exp: Expression,
                  lineCol: LineCol) extends Statement {
  override def hashCode(): Int =
    if (exp != null)
      exp.hashCode
    else
      0

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    exp == obj.asInstanceOf[Return].exp
  }

  override def toString: String = s"($exp)"
}

/**
  * Definition
  *
  */
trait Definition extends Statement

case class VariableDef(name: String,
                       modifiers: Set[Modifier],
                       var vType: Access,
                       var init: Expression,
                       annos: Set[Anno],
                       lineCol: LineCol
                      ) extends Definition with Expression {
  override def hashCode(): Int = {
    var result = name.hashCode
    result = 31 * result + (if (vType != null) vType.hashCode() else 0)
    result = 31 * result + (if (init != null) init.hashCode() else 0)
    result = 31 * result + modifiers.hashCode()
    result = 31 * result + annos.hashCode()
    result
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass) return false
    obj match {
      case o: VariableDef =>
        name == o.name && vType == o.vType && init == o.init && modifiers == o.modifiers && annos == o.annos
    }
  }

  override def toString: String =
    s"VariableDef(${annos.foldLeft("")(_ + " " + _)}${modifiers.foldLeft("")(_ + " " + _)})($name)" +
      s"${if (vType != null) s":$vType"}${if (init != null) s" = $init"}"
}


/**
  * operation
  *
  */
trait Operation extends Expression {
  def operator(): String

  def expressions(): List[Expression]

  def invokeOn(): Int

  def isUnary(): Boolean
}

/**
  * UnaryOneVariableOperation
  *
  */
case class UnaryOneVariableOperation(operator: String,
                                     exp: Expression,
                                     lineCol: LineCol) extends Operation {
  override def expressions(): List[Expression] = List(exp)

  override def invokeOn(): Int = 0

  override def isUnary(): Boolean = true

  override def hashCode(): Int = {
    var result = if (operator != null) operator.hashCode() else 0
    result = 31 * result + (if (exp != null) exp.hashCode() else 0)
    result
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    operator == obj.asInstanceOf[UnaryOneVariableOperation].operator && exp == obj.asInstanceOf[UnaryOneVariableOperation].exp
  }

  override def toString: String =
    s"($operator ${exp})"
}

/**
  * OneVariableOperation
  *
  */
case class OneVariableOperation(operator: String,
                                exp: Expression,
                                lineCol: LineCol) extends Operation {
  override def expressions(): List[Expression] = List(exp)

  override def invokeOn(): Int = 0

  override def isUnary(): Boolean = false

  override def hashCode(): Int = {
    var result = if (operator != null) operator.hashCode() else 0
    result = 31 * result + (if (exp != null) exp.hashCode() else 0)
    result
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    operator == obj.asInstanceOf[UnaryOneVariableOperation].operator && exp == obj.asInstanceOf[UnaryOneVariableOperation].exp
  }

  override def toString: String =
    s"($operator $exp)"
}

/**
  * TwoVariableOperation
  *
  */
case class TwoVariableOperation(operator: String,
                                exp1: Expression,
                                exp2: Expression,
                                lineCol: LineCol) extends Operation {
  override def expressions(): List[Expression] = List(exp1, exp2)

  override def invokeOn(): Int = 0

  override def isUnary(): Boolean = false

  override def hashCode(): Int = {
    var result = if (operator != null) operator.hashCode() else 0
    result = 31 * result + (if (expressions() != null) expressions().hashCode() else 0)
    result
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case TwoVariableOperation(o, e1, e2, _) =>
        o == operator && e1 == exp1 && e2 == exp2
      case _ =>
        false
    }
  }

  override def toString: String =
    s"($operator $exp1 $operator $exp2)"
}