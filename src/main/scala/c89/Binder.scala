package c89

import c89.BindType.BindType
import c89.TokenType.TokenType
import c89.ast._

import scala.collection.mutable.ListBuffer

class Binder() {
  val diagnostics: ListBuffer[String] = new ListBuffer()


  def bindExpression(tree: Expression): BindExpression = {
    tree.getKind() match {
      case TokenType.binaryExpression =>
        bindBinaryExpression(tree.asInstanceOf[BinaryNode])
      case TokenType.unaryExpression =>
        bindUnaryExpression(tree.asInstanceOf[UnaryNode])
      case TokenType.numberExpression =>
        bindLiteralExpression(tree.asInstanceOf[LiteralNode])
      case TokenType.expressionTree =>
        bindExpression(tree.asInstanceOf[ExpressionTree].expr)
      case TokenType.braceExpression =>
        bindExpression(tree.asInstanceOf[BraceNode].op)
      case _ =>
        throw new LexerException(s"unexpected syntax ${tree.getKind()}")
    }
  }


  private def bindLiteralExpression(node: LiteralNode): BindExpression = {
    val value = node.value.value match {
      case "true" => true
      case "false" => false
      case x => x.toInt
    }
    BindLiteralExpression(value)
  }

  private def bindBinaryExpression(node: BinaryNode): BindExpression = {
    val boundLeft = bindExpression(node.left)
    val boundRight = bindExpression(node.right)
    val boundOperatorKind = bindBinaryOperatorKind(node.op.tokenType, boundLeft.bindTypeClass, boundRight.bindTypeClass)
    if (boundOperatorKind == null) {
      diagnostics += s"Unary operator ${node.op.value} is not defined for type ${boundLeft.bindTypeClass}"
      return boundLeft
    }
    BindBinaryExpression(boundOperatorKind, boundLeft, boundRight)
  }

  private def bindUnaryExpression(node: UnaryNode): BindExpression = {
    val boundOperand = bindExpression(node.oprand)
    val boundOperatorKind = bindUnaryOperatorKind(node.op.getKind(), boundOperand.bindTypeClass)
    if (boundOperatorKind == null) {
      diagnostics += s"Unary operator ${node.op.asInstanceOf[Tokens].value} is not defined for type ${boundOperand.bindTypeClass}"
      return boundOperand
    }
    BindUnaryExpression(boundOperatorKind, boundOperand)
  }


  private def bindBinaryOperatorKind(tokenType: TokenType,
                                     left: Class[_],
                                     right: Class[_]): BindType = {
    if (left != right
      || (!left.isInstanceOf[Class[_]]
      && !left.isInstanceOf[Class[_]] )) {
      return null
    }
    tokenType match {
      case TokenType.add => BindType.addition
      case TokenType.sub => BindType.subtraction
      case TokenType.plus => BindType.multiplication
      case TokenType.div => BindType.division
      case TokenType.pow => BindType.pow
      case TokenType.mod => BindType.mod
      case TokenType.and => BindType.and
      case TokenType.or => BindType.or
      case TokenType.lt => BindType.lt
      case TokenType.lte => BindType.lte
      case TokenType.gt => BindType.gt
      case TokenType.gte => BindType.gte
      case TokenType.equal => BindType.equal
      case _ =>
        throw new Exception(s"Unexpected binary operator ${tokenType}")
    }
  }


  private def bindUnaryOperatorKind(tokenType: TokenType,
                                    op: Class[_]): BindType = {
    if(!op.isInstanceOf[Class[_]]
      && !op.isInstanceOf[Class[_]])
      return null
    tokenType match {
      case TokenType.add => BindType.identity
      case TokenType.sub => BindType.negation
      case TokenType.not => BindType.not
      case _ =>
        throw new Exception(s"Unexpected unary operator ${tokenType}")
    }
  }

}

abstract class BoundNode {
  def bindTypeClass: Class[_]
}

abstract class BindExpression extends BoundNode {
}

case class BindBinaryExpression(bindType: BindType,
                                boundLeft: BindExpression,
                                boundRight: BindExpression) extends BindExpression {
  override def bindTypeClass: Class[_] = boundLeft.bindTypeClass
}

case class BindUnaryExpression(bindType: BindType,
                               boundOperand: BindExpression) extends BindExpression {
  override def bindTypeClass: Class[_] = boundOperand.bindTypeClass
}

case class BindLiteralExpression(value: AnyVal) extends BindExpression {
  override def bindTypeClass: Class[_] = value.getClass
}
