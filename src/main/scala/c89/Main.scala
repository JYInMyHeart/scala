package c89

import c89.ast.SyntaxTree

import scala.collection.mutable
import scala.io.StdIn

object Main {
  def main(args: Array[String]): Unit = {
    import Printer._
    val variables = new mutable.HashMap[VariableSymbol,AnyVal]()
    var showTree = false
    while (true) {
      print("> ")
      val str = StdIn.readLine()
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
          val tree = SyntaxTree.parse(str)
          if (showTree)
            prettyPrint(tree.root)
          if (tree.diagnostics.reports.nonEmpty) {
            tree.diagnostics.reports.foreach(
              x => colorPrintln(scala.io.AnsiColor.RED, x.toString)
            )
          } else {
            val compilation = new Compilation(tree,variables)
            val result = compilation.evaluate()
            if (!result.diagnosticsBag.isEmpty) {
              result.diagnosticsBag.reports.foreach(
                x => colorPrintln(scala.io.AnsiColor.RED, x.toString)
              )
            } else
              println(result.value)
          }
      }
    }
  }
}
