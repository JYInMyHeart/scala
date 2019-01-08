package c89

import c89.TokenType.{TokenType, _}
import c89.ast._

import scala.io.AnsiColor.{BLUE, BOLD, GREEN, RESET}

class Parser(val lexer: Lexer) {
  private[this] var tokensList: List[Tokens] = List()
  private[this] var position: Int = _
  var diagnostics: List[String] = lexer.diagnostics

  def init() = {
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

  def parseExpression(): Expression = {
    new ExpressionTree(parseTerm(), eat(eof))
  }

  def parseTerm(): Expression = {
    var left = parseFactor()
    while (current.tokenType == add
      || current.tokenType == sub) {
      val operationToken = nextToken
      val right = parseFactor()
      left = new BinaryNode(left, operationToken, right)
    }
    left
  }

  def parseFactor(): Expression = {
    var left = parsePrimaryExpression()
    while (current.tokenType == plus
      || current.tokenType == div) {
      val operationToken = nextToken
      val right = parsePrimaryExpression()
      left = new BinaryNode(left, operationToken, right)
    }
    left
  }

  def parsePrimaryExpression(): Expression = {
    if (current.tokenType == lb) {
      val left = nextToken
      val expression = parseExpression()
      val right = eat(rb)
      return new BraceNode(left, expression, right)
    }
    val numberNode = eat(TokenType.literalInt)
    new NumberNode(numberNode)
  }

  def colorPrint(colorType: String, text: String) =
    print(s"$colorType$BOLD$text$RESET")

  def colorPrintln(colorType: String, text: String) =
    colorPrint(colorType,text + "\r\n")


  def prettyPrint(node: Expression, indent: String = "", isLast: Boolean = true) {
    var indents = indent
    val enable = node.getChildren() != null
    val marker = if (isLast) "└──" else "├──"


    colorPrint(BLUE, indent)
    colorPrint(BLUE, marker)
    colorPrint(BLUE, node.getKind().toString)


    if (node.isInstanceOf[Tokens] && node.asInstanceOf[Tokens].value != null) {
      print(" ")
      colorPrint(GREEN, node.asInstanceOf[Tokens].value)
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
