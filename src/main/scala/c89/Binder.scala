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
    val boundOperator =
      BoundBinaryOperator.bind(
        node.op.tokenType,
        boundLeft.bindTypeClass.getSimpleName,
        boundRight.bindTypeClass.getSimpleName
      )
    if (boundOperator == null) {
      diagnostics += s"Unary operator ${node.op.value} is not defined for type ${boundLeft.bindTypeClass}"
      return boundLeft
    }
    BindBinaryExpression(boundOperator.bindType, boundLeft, boundRight)
  }

  private def bindUnaryExpression(node: UnaryNode): BindExpression = {
    val boundOperand = bindExpression(node.oprand)
    val boundOperatorKind =
      BoundUnaryOperator.bind(
        node.op.getKind(),
        boundOperand.bindTypeClass.getSimpleName
      )
    if (boundOperatorKind == null) {
      diagnostics += s"Unary operator ${node.op.asInstanceOf[Tokens].value} is not defined for type ${boundOperand.bindTypeClass}"
      return boundOperand
    }
    BindUnaryExpression(boundOperatorKind.bindType, boundOperand)
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

sealed class BoundBinaryOperator(val tokenType: TokenType,
                                 val bindType: BindType,
                                 val left: String,
                                 val right: String,
                                 val result: String)

object BoundBinaryOperator {
  private[this] val int = "Integer"
  private[this] val bool = "Boolean"
  private[this] val double = "Double"

  def apply(
             tokenType: TokenType,
             bindType: BindType,
             left: String,
             right: String,
             result: String
           ): BoundBinaryOperator =
    new BoundBinaryOperator(
      tokenType,
      bindType,
      left,
      right,
      result
    )

  private[this] def binaryOperators: List[BoundBinaryOperator] =
    List(
      BoundBinaryOperator(TokenType.add, BindType.addition, int, int, int),
      BoundBinaryOperator(TokenType.sub, BindType.subtraction, int, int, int),
      BoundBinaryOperator(TokenType.div, BindType.division, int, int, double),
      BoundBinaryOperator(TokenType.plus, BindType.multiplication, int, int, double),
      BoundBinaryOperator(TokenType.pow, BindType.pow, int, int, double),
      BoundBinaryOperator(TokenType.mod, BindType.mod, int, int, int),
      BoundBinaryOperator(TokenType.lt, BindType.lt, int, int, bool),
      BoundBinaryOperator(TokenType.gt, BindType.gt, int, int, bool),
      BoundBinaryOperator(TokenType.lte, BindType.lte, int, int, bool),
      BoundBinaryOperator(TokenType.gte, BindType.gte, int, int, bool),
      BoundBinaryOperator(TokenType.equal, BindType.equal, int, int, bool),
      BoundBinaryOperator(TokenType.equal, BindType.equal, bool, bool, bool),
      BoundBinaryOperator(TokenType.notequal, BindType.notequal, int, int, bool),
      BoundBinaryOperator(TokenType.notequal, BindType.notequal, bool, bool, bool),
      BoundBinaryOperator(TokenType.and, BindType.and, bool, bool, bool),
      BoundBinaryOperator(TokenType.or, BindType.or, bool, bool, bool)
    )

  def bind(tokenType: TokenType, left: String, right: String): BoundBinaryOperator = {
    val binaryOperator = binaryOperators.filter(x => x.tokenType == tokenType && x.left == left && x.right == right)
    if (binaryOperator.nonEmpty)
      binaryOperator.last
    else
      null
  }
}

sealed class BoundUnaryOperator(val tokenType: TokenType,
                                val bindType: BindType,
                                val operand: String,
                                val result: String)

object BoundUnaryOperator {
  private[this] val int = "Integer"
  private[this] val bool = "Boolean"

  def apply(
             tokenType: TokenType,
             bindType: BindType,
             operand: String,
             result: String
           ): BoundUnaryOperator = new BoundUnaryOperator(
    tokenType,
    bindType,
    operand,
    result
  )

  private[this] def unaryOperators: List[BoundUnaryOperator] =
    List(
      BoundUnaryOperator(TokenType.not, BindType.not, bool, bool),
      BoundUnaryOperator(TokenType.sub, BindType.negation, int, bool),
      BoundUnaryOperator(TokenType.add, BindType.identity, int, bool)
    )

  def bind(tokenType: TokenType, operand: String): BoundUnaryOperator = {
    val unaryOperator = unaryOperators.filter(x => x.tokenType == tokenType && x.operand == operand)
    if (unaryOperator.nonEmpty)
      unaryOperator.last
    else
      null
  }
}


