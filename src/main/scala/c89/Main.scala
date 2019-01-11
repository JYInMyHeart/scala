package c89

import c89.Lexer.newLexer

import scala.io.StdIn

object Main {
  def exec(str: String): Unit = {
    val lexer = newLexer(str)
    parse(lexer, showTree = true)
  }

  def parse(lexer: Lexer, showTree: Boolean): Unit = {

  }


  def main(args: Array[String]): Unit = {
    import Printer._
    var showTree = false
    while (true) {
      print("> ")
      val str = StdIn.readLine()
      var lexer: Lexer = null
      str match {
        case "q" => System.exit(0)
        case "show" => showTree = !showTree
          if (showTree)
            println("show ast!")
          else
            println("not show ast!")
        case "h" =>
          println(
            """h:help
              |q:exit
              |show:show ast?
            """.stripMargin)
        case _ =>
          lexer = newLexer(str)
          val parser = new Parser(lexer)
          parser.init()
          val tree = parser.parseTreeExpression()
          if (showTree)
            prettyPrint(tree)
          if (parser.diagnostics.reports.nonEmpty) {
            parser.diagnostics.reports.foreach(
              x => colorPrintln(scala.io.AnsiColor.RED, x.toString)
            )
          } else {
            val binder = new Binder
            val bindTree = binder.bindExpression(tree)
            if (binder.diagnostics.reports.nonEmpty) {
              binder.diagnostics.concat(parser.diagnostics)
              binder.diagnostics.reports.foreach(
                x => colorPrintln(scala.io.AnsiColor.RED, x.toString)
              )
            } else {
              val eval = new Eval(bindTree).eval()
              println(eval)
            }
          }
      }
    }
  }
}
