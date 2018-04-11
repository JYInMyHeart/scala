package tpye

/**
  * @author xck
  */
object Parser {
  def read(exp:String) : XckType = readStr(exp)
  def eval(exp:XckType): XckType = exp
  def printXck(exp:XckType) = Printer.prStr(exp)
  def repl(exp:String) = printXck(eval(read(exp)))

  def main(args: Array[String]): Unit = {
    while(true){
      print("xck>")
      val exp = Console.in.readLine()
      exp match {
        case "EOF" => System.exit(1)
        case _ => repl(exp)
      }
    }
  }

}
