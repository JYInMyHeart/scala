package c89

import java.io.{ByteArrayInputStream, PushbackInputStream}

import TokenType._

class Lexer(val pushbackInputStream: PushbackInputStream) {
  var current:Tokens = _
  var ch: Char = _
  var lineCount: Int = 1
  var columnCount: Int = _

  def read() = {
    val c = pushbackInputStream.read()
    ch = c.toChar
    columnCount += 1
  }

  def unRead(c: Char) = {
    pushbackInputStream.unread(c.toInt)
  }

  def nextToken(): Tokens = {
    read()
    current = ch match {
      case x if x == ' ' =>
        Tokens(whiteSpace, null, lineCount, columnCount)
      case x if x == '\n' | x == '\r' =>
        lineCount += 1
        columnCount = 0
        Tokens(newline, null, lineCount, columnCount)
      case x if x < '9' && x > '0' =>
        Tokens(literalInt, getNum, lineCount, columnCount)
      case x if x == '_' || Character.isLetter(x) =>
        Tokens(identifier, getChars, lineCount, columnCount)
      case '+' => Tokens(add, "+", lineCount, columnCount)
      case '*' => Tokens(plus, "*", lineCount, columnCount)
      case '/' => Tokens(div, "/", lineCount, columnCount)
      case '%' => Tokens(mod, "%", lineCount, columnCount)
      case '^' => Tokens(pow, "^", lineCount, columnCount)
      case '&' => Tokens(and, "&", lineCount, columnCount)
      case '|' => Tokens(or, "|", lineCount, columnCount)
      case '!' => Tokens(not, "!", lineCount, columnCount)
      case x if x == '-' =>
        read()
        var token: Tokens = null
        ch match {
          case '>' =>
            token = Tokens(keyword, "->", lineCount, columnCount)
          case _ =>
            unRead(ch)
            token = Tokens(sub, "-", lineCount, columnCount)
        }
        token
      case x if x == '\"' =>
        Tokens(literalStr, getStr, lineCount, columnCount)
      case '(' =>
        Tokens(lb, "(", lineCount, columnCount)
      case ')' =>
        Tokens(rb, ")", lineCount, columnCount)
      case '<' =>
        read()
        ch match {
          case '=' =>
            Tokens(lte, "<=", lineCount, columnCount)
          case _ =>
            unRead(ch)
            Tokens(lt, "<", lineCount, columnCount)
        }
      case '>' =>
        read()
        ch match {
          case '=' =>
            Tokens(gte, ">=", lineCount, columnCount)
          case _ =>
            unRead(ch)
            Tokens(gt, ">", lineCount, columnCount)
        }
      case '=' =>
        read()
        ch match {
          case '=' =>
            Tokens(equal, "==", lineCount, columnCount)
          case _ =>
            unRead(ch)
            Tokens(assign, "=", lineCount, columnCount)
        }
      case '\0' =>
        Tokens(eof, "EOF", lineCount, columnCount)
      case _ => throw new LexerException("unknown token")
    }
    current
  }


  def getNum: String = {
    var str: String = ""
    while (Character.isDigit(ch)) {
      str += ch.toString
      read()
    }
    unRead(ch)
    str
  }

  def getStr: String = {
    var str: String = ""
    read()
    while (ch != '\"') {
      str += ch.toString
      read()
    }
    str
  }

  def getChars: String = {
    var str: String = ""
    while (Character.isLetter(ch) || Character.isDigit(ch) || ch == '_') {
      str += ch.toString
      read()
    }
    unRead(ch)
    str
  }
}

object Lexer {

  def lexer(expr: String) = {
    val lexer = new Lexer(new PushbackInputStream(new ByteArrayInputStream((expr + '\0').getBytes)))
    while (lexer.pushbackInputStream.available() > 0) {
      val token = lexer.nextToken()
      println(token)
    }
  }

  def main(args: Array[String]): Unit = {
    lexer("\"afa2t2\"+3-4*5-( 6+7 ) -> 3 - asfa_141")
  }
}
