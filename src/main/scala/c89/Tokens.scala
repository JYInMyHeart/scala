package c89

import c89.TokenType.TokenType

class Tokens(val tokenType: TokenType, val value: String, var line: Int, var column: Int) {
  override def toString: String = s"$value :$tokenType at $line ,$column"

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
  val keyword, literalInt, func,identifier ,literalStr,lb,rb ,assign,compare,op = Value
}


