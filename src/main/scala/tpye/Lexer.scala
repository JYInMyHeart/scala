package tpye

import java.util.regex.Pattern

/**
  * @author xck
  */
class Lexer(var pos: Int)(var tokens: List[String]) {
  def readStr(exp: String): XckType = {
    //    val lexer = Lexer(0)(List[String])
    pos = 0
    tokenizer(exp)(tokens)
    readForm
  }

  def tokenizer(exp: String)(tokens: List[String]) = {
    val pattern = Pattern.compile("[\\s ,]*(~@|[\\[\\]{}()'`~@]|\"(?:[\\\\].|[^\\\\\"])*\"|;.*|[^\\s \\[\\]{}()'\"`~@,;]*)")
    val matcher = pattern.matcher(exp)
    while ( {
      matcher.find
    }) {
      val token = matcher.group(1)
      if (token != null && !(token == "") && !(token.charAt(0) == ';')) tokens ++ token
    }
  }


  def readForm: XckType = {
    tokens.map(x => x)

    def readToken(token: String): XckType = token.charAt(0) match {
      case '\'' =>
        return new Types.MalList(new Types.MalSymbol("quote"), readForm)
      case '`' =>
        return new Types.MalList(new Types.MalSymbol("quasiquote"), readForm)
      case '~' =>
        if (token == "~") {

          return new Types.MalList(new Types.MalSymbol("unquote"), readForm)
        }
        else {

          return new Types.MalList(new Types.MalSymbol("splice-unquote"), readForm)
        }
      case '@' =>

        return new Types.MalList(new Types.MalSymbol("deref"), readForm)
      case '(' =>
        form = readList(new Types.MalList, '(', ')')
        break //todo: break is not supported
      case ')' =>
        System.out.print("unexpected ')'")
      case '[' =>
        form = readList(new Types.MalVector, '[', ']')
        break //todo: break is not supported
      case ']' =>
        println("unexpected ']'")
      case _ =>
        form = readAtom
    }

  }

}

object Lexer {

}

