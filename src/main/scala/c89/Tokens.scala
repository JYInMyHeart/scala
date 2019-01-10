package c89

import c89.TokenType.TokenType
import c89.ast.Expression

class Tokens(val tokenType: TokenType,
             val value: String,
             var line: Int,
             var column: Int) extends Expression {
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
  falseKeyword,
  trueKeyword,


  //expressions
  binaryExpression,
  numberExpression,
  unaryExpression,
  expressionTree,
  braceExpression

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
  equal
  = Value
}


