package c89

import c89.Lexer.newLexer

import scala.io.AnsiColor.RED

object Main extends App {
  val lexer = newLexer("(1 + 2")
  val parser = new Parser(lexer)
  parser.init()
  val tree = parser.parseExpression()
  parser.prettyPrint(tree)
  if (parser.diagnostics.nonEmpty) {
    for (d <- parser.diagnostics) {
      parser.colorPrintln(RED, d)
    }
  }
}
