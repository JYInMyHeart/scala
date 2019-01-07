package c89

import c89.TokenType.TokenType

class Tokens(val tokenType: TokenType, val value: String, var line: Int, var column: Int) {
  override def toString: String = s"<$tokenType :$value >  line$line,col$column"

}

object Tokens {
  def apply(tokenType: TokenType,
            value: String,
            line: Int,
            column: Int
           ): Tokens = new Tokens(tokenType, value, line, column
  )
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
  eof = Value
}


