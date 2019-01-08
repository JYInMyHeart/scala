package c89

import java.io.PushbackInputStream

import c89.Lexer.newLexer

import scala.io.AnsiColor.RED

object Main {
  def exec(str: String) = {
    val lexer = newLexer(str)
    parse(lexer)
  }

  def parse(lexer: Lexer) = {
    val parser = new Parser(lexer)
    parser.init()
    val tree = parser.parseTreeExpression()
    parser.prettyPrint(tree)
    if (parser.diagnostics.nonEmpty) {
      for (d <- parser.diagnostics) {
        parser.colorPrintln(RED, d)
      }
    }
    tree
  }


  def main(args: Array[String]): Unit = {
    val lexer = new Lexer(new PushbackInputStream(System.in))
    val tree = parse(lexer)
    val eval = new Eval(tree).eval()
    println(eval)
  }
}
