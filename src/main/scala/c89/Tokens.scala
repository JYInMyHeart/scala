package c89

import c89.TokenType.TokenType
import c89.ast.Expression

class Tokens(val tokenType: TokenType,
             val value: String,
             var line: Int,
             var column: Int) extends Expression{
  override def toString: String =
    s"<$tokenType :$value >  line$line,col$column"

  override def getKind(): TokenType = tokenType

  override def getChildren(): List[Expression] = null
}

object Tokens {
  def apply(tokenType: TokenType,
            value: String,
            line: Int,
            column: Int
           ): Tokens =
    new Tokens(tokenType, value, line, column)
}

object TokenType extends Enumeration {
  type TokenType = Value
  val keyword,
  literalInt,
  func,
  identifier,
  literalStr,
  lb,
  rb,
  assign,
  equal,
  lt,
  gt,
  lte,
  gte,
  add,
  sub,
  plus,
  div,
  mod,
  and,
  or,
  not,
  pow,
  whiteSpace,
  newline,
  eof,
  wrong,


  binaryExpression,
  numberExpression,
  expressionTree,
  braceExpression

  = Value
}


