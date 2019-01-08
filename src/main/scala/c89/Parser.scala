package c89

import c89.TokenType.{TokenType, _}
import c89.ast._

import scala.io.AnsiColor.{BLUE, BOLD, GREEN, RESET}

class Parser(val lexer: Lexer) {
  private[this] var tokensList: List[Tokens] = List()
  private[this] var position: Int = _
  var diagnostics: List[String] = lexer.diagnostics

  def init(): Unit = {
    var token = lexer.nextToken()
    while (token.tokenType != eof && token.tokenType != wrong) {
      if (
        token.tokenType != whiteSpace)
        tokensList :+= token
      token = lexer.nextToken()
    }
  }


  def peek(offset: Int): Tokens = {
    val size = tokensList.length
    val index: Int = position + offset
    if (index >= size) {
      return tokensList(size - 1)
    }
    tokensList(index)
  }

  def current: Tokens = peek(0)

  private def nextToken = {
    val currentToken = current
    position += 1
    currentToken
  }

  def eat(tokenType: TokenType): Tokens = {
    if (tokenType == current.tokenType)
      return nextToken
    diagnostics :+= s"error:expected a $tokenType here"
    Tokens(tokenType, null, current.line, current.column)
  }

  def parseTreeExpression(): Expression = {
    new ExpressionTree(parseExpression())
  }

  def getBinaryOperatorPrecedence(tokenType: TokenType): Int = {
    tokenType match {
      case x if x == TokenType.add
        | x == TokenType.sub =>
        1
      case x if x == TokenType.div
        | x == TokenType.mod
        | x == TokenType.plus
        | x == TokenType.pow =>
        2
      case _ =>
        0
    }
  }

  def getUnaryOperatorPrecedence(tokenType: TokenType): Int =
    tokenType match {
      case x if x == TokenType.add
        | x == TokenType.sub =>
        3
      case _ =>
        0
    }


  def parseExpression(parentPrecedence: Int = 0): Expression = {
    var left: Expression = null
    val unaryOperatorPrecedence = getUnaryOperatorPrecedence(current.tokenType)
    if (unaryOperatorPrecedence != 0 && unaryOperatorPrecedence >= parentPrecedence) {
      val operatorToken = nextToken
      val operand = parseExpression(unaryOperatorPrecedence)
      return new UnaryNode(operatorToken, operand)
    } else
      left = parsePrimaryExpression()
    while (true) {
      val precedence = getBinaryOperatorPrecedence(current.tokenType)
      if (precedence == 0 || precedence <= parentPrecedence)
        return left
      val operatorToken = nextToken
      val right = parseExpression(precedence)
      left = new BinaryNode(left, operatorToken, right)
    }
    left
  }


  def parsePrimaryExpression(): Expression = {
    if (current.tokenType == lb) {
      val left = nextToken
      val expression = parseTreeExpression()
      val right = eat(rb)
      return new BraceNode(left, expression, right)
    }
    val numberNode = eat(TokenType.literalInt)
    new NumberNode(numberNode)
  }

  def colorPrint(colorType: String, text: String): Unit =
    print(s"$colorType$BOLD$text$RESET")

  def colorPrintln(colorType: String, text: String): Unit =
    colorPrint(colorType, text + "\r\n")


  def prettyPrint(node: Expression, indent: String = "", isLast: Boolean = true) {
    var indents = indent
    val enable = node.getChildren() != null
    val marker = if (isLast) "└──" else "├──"


    colorPrint(BLUE, indent)
    colorPrint(BLUE, marker)
    colorPrint(BLUE, node.getKind().toString)


    node match {
      case tokens: Tokens if tokens.value != null =>
        print(" ")
        colorPrint(GREEN, tokens.value)
      case _ =>
    }

    println()

    indents += (if (isLast) "    " else "│   ")


    if (enable) {
      val last = node.getChildren().last
      for (child <- node.getChildren())
        prettyPrint(child, indents, child == last)
    }

  }

}
