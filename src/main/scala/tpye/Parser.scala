package tpye

import tpye.Environment.Environment
import tpye.Types._

/**
  * @author xck
  */
object Parser {
  def read(exp: String): XckType = new Lexer(0)(new Array[String](0)).readStr(exp)

  def eval(exp: XckType, environment: Environment): XckType = {
//    while (true) {
//      if (!exp.isInstanceOf[XckList])
//        evalAst(exp, environment)
//      exp.asInstanceOf[XckList].list match {
//        case Nil => return exp
//        case new XckSymbol("if") :: a1 :: a2 :: Nil => {
//
//      }
//
//      }
//
//        case "if" =>
//          eval(ast.nth(1), environment).asInstanceOf[XckBoolean].value match {
//            case true => return eval(ast.nth(2), environment)
//            case false => return eval(ast.nth(3), environment)
//          }
//        case "lambda" =>
//          val valBinds = ast.nth(1).asInstanceOf[XckList]
//          val valExprs = ast.nth(2).asInstanceOf[XckList]
//          return new XckFunc(valExprs, environment, valBinds) {
//            override def apply(a: XckList): XckType =
//              eval(valExprs, getEnvironment(environment, valBinds, a))
//          }
//        case _ =>
//          val funcVal = evalAst(ast, environment).asInstanceOf[XckList]
//          val f = funcVal.nth(0).asInstanceOf[XckFunc]
//          f.ast match {
//            case null => return f.apply(funcVal.slice(1))
//            case _ => return eval(f.ast, f.getEnvironment(funcVal.slice(1)))
//          }
//
//      }
    exp

  }


  def printXck(exp: XckType): Unit = Printer.prStr(exp)

  def repl(exp: String): Unit = printXck(eval(read(exp), null))

  def main(args: Array[String]): Unit = {
    while (true) {
      try {
        print("xck>")
        val exp = Console.in.readLine()
        exp match {
          case "EOF" => System.exit(1)
          case _ => repl(exp)
        }
      } catch {
        case e => println(e)
      }
    }
  }

  def evalAst(xck: XckType, env: Environment): XckType =
//    xck match {
//      case exprs: XckList =>
//        var values = exprs
//        for (i <- 0 to exprs.)
//          values.list = values.list.:+(eval(exprs.nth(i), env))
//        values
//      case _ => xck match {
//        case xckSymbol: XckSymbol =>
//          env.get(xckSymbol)
//        case _ => xck
//      }

//    }
xck

}
