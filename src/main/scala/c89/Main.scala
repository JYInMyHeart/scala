package c89

import c89.Lexer.newLexer

import scala.io.AnsiColor.RED
import scala.io.StdIn

object Main {
  def exec(str: String) = {
    val lexer = newLexer(str)
    parse(lexer,true)
  }

  def parse(lexer: Lexer,showTree:Boolean) = {
    val parser = new Parser(lexer)
    parser.init()
    val tree = parser.parseTreeExpression()
    if(showTree)
      parser.prettyPrint(tree)
    if (parser.diagnostics.nonEmpty) {
      for (d <- parser.diagnostics) {
        parser.colorPrintln(RED, d)
      }
    }
    tree
  }


  def main(args: Array[String]): Unit = {
    var showTree = false
    while(true){
      print("> ")
      val str = StdIn.readLine()
      var lexer:Lexer = null
      str match {
        case "q" => System.exit(0)
        case "show" => showTree = !showTree
          if(showTree)
            println("show ast!")
          else
            println("not show ast!")
        case "h" =>
          println("""h:help
            |q:exit
            |show:show ast?
          """.stripMargin)
        case _ =>
          lexer = newLexer(str)
          val tree = parse(lexer,showTree)
          val eval = new Eval(tree).eval()
          println(eval)
      }

    }

  }
}
