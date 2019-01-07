package c89

import TokenType.{TokenType, _}
class Parser (val lexer: Lexer){

  sealed abstract class Ast{
    def getKind():Tokens
  }
  sealed abstract class Expression extends Ast
  sealed class BinaryExpression(val left:Ast,val op:Tokens,val right:Ast) extends Expression {
    override def getKind(): Tokens = op
  }
  sealed class ArrowExpression(val identifier:Ast,val body:Ast) extends Expression {
    override def getKind(): Tokens = ???
  }
  sealed class PrimaryExpression(val value:Tokens) extends Expression{
    override def getKind(): Tokens = value
  }

  def eat(tokenType: TokenType,token:Tokens):Boolean = {
    tokenType == token.tokenType
  }

  def parseExpression():Expression = {
    null
  }

  def parse(): Unit ={
    val root = parseExpression()
    eat(eof,lexer.nextToken())
  }

}
