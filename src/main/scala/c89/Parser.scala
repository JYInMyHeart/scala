package c89

import TokenType.{TokenType, _}

class Parser(val lexer: Lexer) {


  sealed abstract class Ast {
    def getKind(): TokenType
    def getChildren():List[Expression]
  }

  sealed abstract class Expression extends Ast

  sealed class BinaryNode(val left: Expression, val op: Tokens, val right: Expression) extends Expression {
    override def getKind() = op.tokenType

    override def getChildren(): List[Expression] = List()
  }

  sealed class NumberNode(val value: Tokens) extends Expression {
    override def getKind() = value.tokenType

    override def getChildren(): List[Expression] = value
  }


  def eat(tokenType: TokenType): Tokens = {
    if (tokenType == lexer.current.tokenType)
      lexer.nextToken()
    Tokens(tokenType, lexer.current.value, lexer.current.line, lexer.current.column)
  }

  def parseExpression(): Expression = {
    null
  }

  def parse(): Expression = {
    var left = parsePrimaryExpression()
    while (lexer.current.tokenType == add ||
      lexer.current.tokenType == sub) {
      val operationToken = lexer.nextToken()
      val right = parsePrimaryExpression()
      left = new BinaryNode(left, operationToken, right)
    }
    left
  }

  def parsePrimaryExpression(): Expression = {
    val numberNode = eat(TokenType.numberExpression)
    new NumberNode(numberNode)
  }

}
