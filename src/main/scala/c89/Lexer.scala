package c89

import java.io.{ByteArrayInputStream, PushbackInputStream}

import TokenType._

class Lexer(val pushbackInputStream: PushbackInputStream) {
  var ch: Char = _
  var lineCount: Int = _
  var columnCount: Int = _

  def read() = {
    ch = pushbackInputStream.read().toChar
    columnCount += 1
  }

  def unRead(c: Char) = {
    pushbackInputStream.unread(c.toInt)
  }

  //  def spaces() = {
  //    ch match {
  //      case x if x == ' '  =>
  //        columnCount += 1
  //      case x if x == '\n' | x == '\r' =>
  //        lineCount += 1
  //        columnCount = 0
  //    }
  //  }

  def end = pushbackInputStream.available() <= 0

  def lexer(): List[Tokens] = {
    var tokens = List[Tokens]()
    while (!end) {
      read()
      ch match {
        case x if x == ' ' =>

        case x if x == '\n' | x == '\r' =>
          lineCount += 1
          columnCount = 0
        case x if x < '9' && x > '0' =>
          val token = Tokens(literalInt, getNum, lineCount, columnCount)
          tokens :+= token
        case x if x == '_' || Character.isLetter(x) =>
          val token = Tokens(identifier, getChars, lineCount, columnCount)
          tokens :+= token
        case '+' => tokens :+= Tokens(op, "+", lineCount, columnCount)
        case '*' => tokens :+= Tokens(op, "*", lineCount, columnCount)
        case '/' => tokens :+= Tokens(op, "/", lineCount, columnCount)
        case '%' => tokens :+= Tokens(op, "%", lineCount, columnCount)
        case '^' => tokens :+= Tokens(op, "^", lineCount, columnCount)
        case '&' => tokens :+= Tokens(op, "&", lineCount, columnCount)
        case '|' => tokens :+= Tokens(op, "|", lineCount, columnCount)
        case '!' => tokens :+= Tokens(op, "!", lineCount, columnCount)

        case x if x == '-' =>
          read()
          var token: Tokens = null
          ch match {
            case '>' =>
              token = Tokens(keyword, "->", lineCount, columnCount)
//            case x if Character.isDigit(x) =>
//              token = Tokens(literalInt, "-" + getNum, lineCount, columnCount)
            case _ =>
              unRead(ch)
              token = Tokens(op, "-", lineCount, columnCount)

          }
          tokens :+= token
        case x if x == '\"' =>
          tokens :+= Tokens(literalStr, getStr, lineCount, columnCount)
        case '(' =>
          tokens :+= Tokens(lb, "(", lineCount, columnCount)
        case ')' =>
          tokens :+= Tokens(rb, ")", lineCount, columnCount)
        case '<' =>
          read()
          ch match {
            case '=' =>
              tokens :+= Tokens(compare, "<=", lineCount, columnCount)
            case _ =>
              unRead(ch)
              tokens :+= Tokens(compare, "<", lineCount, columnCount)
          }
        case '>' =>
          read()
          ch match {
            case '=' =>
              tokens :+= Tokens(compare, ">=", lineCount, columnCount)
            case _ =>
              unRead(ch)
              tokens :+= Tokens(compare, ">", lineCount, columnCount)
          }
        case '=' =>
          read()
          ch match {
            case '=' =>
              tokens :+= Tokens(compare, "==", lineCount, columnCount)
            case _ =>
              unRead(ch)
              tokens :+= Tokens(assign, "=", lineCount, columnCount)
          }


        case _ => throw new LexerException("unknown token")
      }
    }
    tokens
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

object Lexer{
  def main(args: Array[String]): Unit = {

    val lexer = new Lexer(new PushbackInputStream(new ByteArrayInputStream("\"22342\"".getBytes)))
    val list = lexer.lexer()
    println(list)
  }
}
