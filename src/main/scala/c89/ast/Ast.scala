package c89.ast

import c89.TokenType._
import c89.Tokens

abstract class Ast {
  def getKind(): TokenType

  def getChildren(): List[Expression]
}

abstract class Expression extends Ast

sealed class ExpressionTree(val expr:Expression) extends Expression{
  override def getKind(): TokenType = expressionTree

  override def getChildren(): List[Expression] = List[Expression](expr)

  override def toString: String = s"ExpressionTree:$expr"
}

sealed class BinaryNode(val left: Expression,
                        val op: Tokens,
                        val right: Expression) extends Expression {
  override def getKind(): TokenType = binaryExpression

  override def getChildren(): List[Expression] = {
    List[Expression](left,op,right)
  }

  override def toString: String = s"BinaryNode:${left.getKind()}"
}

sealed class NumberNode(val value: Tokens) extends Expression {
  override def getKind(): TokenType = numberExpression

  override def getChildren(): List[Expression] = {
    List[Expression](value)
  }
}

sealed class BraceNode(val left:Expression,
                       val op:Expression,
                       val right: Expression) extends Expression {
  override def getKind(): TokenType = braceExpression

  override def getChildren(): List[Expression] = List(left,op,right)

}

sealed class UnaryNode(val op:Expression,
                       val oprand:Expression) extends Expression{
  override def getKind(): TokenType = unaryExpression

  override def getChildren(): List[Expression] = List[Expression](op,oprand)
}
