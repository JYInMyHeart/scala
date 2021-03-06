package c89

import c89.TokenType.TokenType
import c89.ast.Expression

class Tokens(val tokenType: TokenType,
             val value: String,
             val span: Span) extends Expression {
  override def toString: String =
    s"<$tokenType :$value >  $span"

  override def getKind(): TokenType = tokenType

  override def getChildren(): List[Expression] = null
}

object Tokens {
  def apply(tokenType: TokenType,
            value: String,
            span: Span
           ): Tokens =
    new Tokens(tokenType, value, span)
}

object TokenType extends Enumeration {
  type TokenType = Value
  val
  //tokens
  keyword,
  func,
  identifier,
  literal,
  lb,
  rb,
  assign,
  equal,
  notequal,
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
  equalsToken,

  //keyword
  falseKeyword,
  trueKeyword,


  //expressions
  binaryExpression,
  numberExpression,
  unaryExpression,
  expressionTree,
  braceExpression,
  nameExpression,
  assignmentExpression

  = Value
}

object BindType extends Enumeration {
  type BindType = Value
  val identity,
  negation,
  addition,
  subtraction,
  multiplication,
  division,
  and,
  or,
  not,
  pow,
  mod,
  lt,
  lte,
  gt,
  gte,
  equal,
  notequal
  = Value
}


