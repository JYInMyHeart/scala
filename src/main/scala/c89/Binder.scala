package c89

import c89.TokenType.TokenType
import c89.ast.{BinaryNode, Expression, NumberNode, UnaryNode}

import scala.collection.mutable.ListBuffer

class Binder() {
  val diagnostics: ListBuffer[String] = new ListBuffer()


  def bindExpression(tree: Expression): BindExpression = {
    tree.getKind() match {
      case TokenType.binaryExpression =>
        BindBinaryExpression(tree.asInstanceOf[BinaryNode])
      case TokenType.unaryExpression =>
        BindUnaryExpression(tree.asInstanceOf[UnaryNode])
      case TokenType.numberExpression =>
        BindLiteralExpression(tree.asInstanceOf[NumberNode])
      case _ =>
        throw new LexerException(s"unexpected syntax ${tree.getKind()}")
    }
  }

  sealed class BindExpression {

  }

  case class BoundBinaryExpression(tokenType: TokenType,
                                   boundLeft: BindExpression,
                                   boundRight: BindExpression) extends BindExpression
  case class BoundUnaryExpression(tokenType: TokenType,
                                  boundOpearnd: BindExpression) extends BindExpression

  private def BindLiteralExpression(node: NumberNode): BindExpression = {
    val value = node.value.value match {
      case "true" => true
      case "false" => false
      case x => x.toInt
    }
    BindLiteralExpression(value)
  }

  private def BindBinaryExpression(node: BinaryNode): BindExpression = {
    val boundLeft = bindExpression(node.left)
    val boundRight = bindExpression(node.right)
    val boundOperatorKind = bindBinaryOperatorKind(node.op.tokenType, boundLeft.getClass, boundRight.getClass)
    if (boundOperatorKind == null) {
      diagnostics += s"Unary operator ${node.op.value} is not defined for type ${boundLeft.getClass}"
      return boundLeft
    }
    BoundBinaryExpression(boundOperatorKind.tokenType, boundLeft, boundRight)
  }

  private def BindUnaryExpression(node: UnaryNode): BindExpression = {
    val boundOpearnd = bindExpression(node.op)
    val boundOperatorKind = BindUnaryOperatorKind(node.op.getKind(), boundOpearnd.getClass)
    if (boundOperatorKind == null) {
      diagnostics += s"Unary operator ${node.op.asInstanceOf[Tokens].value} is not defined for type ${boundOpearnd.getClass}"
      return boundOpearnd
    }
    BoundUnaryExpression(boundOperatorKind.tokenType, boundOpearnd)
  }



  case class BindLiteralExpression(value: AnyVal) extends BindExpression


  private def bindBinaryOperatorKind(tokenType: TokenType,
                                    left: Class[_ <: BindExpression],
                                    right: Class[_ <: BindExpression]) = {

  }



  private def BindUnaryOperatorKind(tokenType: TokenType,
                                   op: Class[_ <: BindExpression]) = {

  }



}
