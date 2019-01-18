package latte
import java.io.BufferedReader

case class Lexer(fileName:String,
                 reader:BufferedReader,
                 indent:Int) {
  def parse:ElementStartNode = {
    val args:Args = Args()
    args.fileName = fileName
    val elementStartNode = ElementStartNode(args,0)
    args.startNodeStack.push(elementStartNode)
    parse(args)
    finalCheck(elementStartNode)
    elementStartNode
  }
  import Lexer._
  def parse(args: Args):Unit = {
    val line = reader.readLine()
    var rootIndent = -1
    while(line != null){
      args.currentLine += 1

      line match {
        case l if l.startsWith("define") =>
          if(!line.equals("define")
            && SPLIT.contains(l.substring("define".length,"define".length + 1))){
            args.currentCol += 1

            val originalString = l
            var anotherLine = l.substring("define".length)
            args.currentCol += "define".length

            val las1 = getStringFroProcessing(anotherLine,args,originalString,"define")
            anotherLine = las1.line
            val target = las1.str.substring(1,las1.str.length - 1)
            target match {
              case x if x.isEmpty =>
                throw new Exception(s"define <target> length cannot be 0 at ${las1.lineCol}")
              case x if x.contains(ESCAPE) =>
                throw new Exception(s"define <target> cannot contain escape char at ${las1.lineCol}")
              case x if !x.trim.startsWith("as") =>
                throw new Exception(
                  s"""illegal define command
                     |(there should be an `as` between <target> and <replacement>)
                     | at ${args.generateLineCol}""".stripMargin)
              case _ =>
                throw new Exception(s"unknown exception at ${las1.lineCol}")
            }
            var asPos = anotherLine.indexOf("as")
            anotherLine = anotherLine.substring(asPos + 2)
            anotherLine match {
              case x if x.isEmpty =>
                throw new Exception(s"illegal define command $originalString at ${args.generateLineCol}")
              case x if !x.contains(x.charAt(0).toString) =>
                throw new Exception(
                  s"""illegal define command
                     |(there should be an `as` between <target> and <replacement>)
                     | at ${args.generateLineCol}""".stripMargin)
              case _ =>
                throw new Exception(s"unknown exception at ${las1.lineCol}")
            }

            args.currentCol += asPos + 2
            val las2 = getStringFroProcessing(anotherLine,args,originalString,"define")
            anotherLine = las2.line
            val replacement = las2.str.substring(1,las2.str.length - 1)

            if (replacement.contains(ESCAPE))
              throw new Exception(s"define <replacement> cannot contain escape char at ${las1.lineCol}")
            if (!anotherLine.trim.isEmpty)
              throw new Exception(
                s"""illegal define command
                   |(there should not be any characters between after <replacement>)
                   | at ${args.generateLineCol}""".stripMargin)

            args.defined += target -> replacement
            anotherLine = reader.readLine()
            args.currentCol = 0
          }
        case x if x.startsWith("undef") =>
          if(x != "undef"
            && SPLIT.contains(x.substring("undef".length,"undef".length + 1))){

          }
        case _ =>

      }

    }
  }


  private def getStringFroProcessing(line:String,
                             args: Args,
                             originalString:String,
                             command:String):LineAndString = {
    val str = line.trim
    if(str.isEmpty)
      throw new Exception(s"illegal $command command $originalString at ${args.generateLineCol}")
    var token = str.charAt(0).toString
    if(!STRING.contains(token))
      throw new Exception(s"illegal $command command $originalString at ${args.generateLineCol}")
    args.currentCol += line.indexOf(token)
    var newLine = str
    var lastIndex = 0
    val lineCol = args.generateLineCol
    var enable = true
    while(enable){
      val index = newLine.indexOf(token,lastIndex + 1)
      if(newLine.length <= 1 || index == -1)
        throw new Exception(s"end of string not found at $lineCol")
      val c = newLine.charAt(index - 1)
      if(ESCAPE != c.toString){
        val s = newLine.substring(0,index + 1)
        args.currentCol += (index + token.length)
        newLine = newLine.substring(index + 1)
        token = s
        enable = false
      }else
        lastIndex = index
    }
    val las = LineAndString()
    las.line = newLine
    las.str = token
    las.lineCol = lineCol
    las
  }

  private def finalCheck(root:ElementStartNode) = {

  }
}

private case class LineAndString() {
  var str: String = _
  var line: String = _
  var lineCol: LineCol = _
}


object Lexer {
  val LAYER: Set[String] = Set("#>", "#", "->")
  val STRING: Set[String] = Set("\"", "'", "`")
  val NO_RECORD: Set[String] = Set(" ")
  val ESCAPE:String = "\\"
  val ENDING = ","
  val COMMENT = ";"
  var PAIR: Map[String, String] = Map(
    "(" -> ")",
    "{" -> "}",
    "[" -> "]",
  )
  val SPLITS: Set[String] = Set(
    ".",
    ":",
    "::",
    "=", "+=", "-=", "*=", "/=", "%=",
    ">>", "<<", ">>>",
    "&", "^", "|", "~",
    "^^",
    "!", "&&", "||",
    "!=", "==", "!==", "===",
    "<", ">", "<=", ">=",
    "+", "-", "*", "/", "%",
    "++", "--",
    "@",
    "=:=", "!:=",
    "..", ".:",
    "..."
  ) ++ NO_RECORD
  var SPLIT: List[String] = STRING.toList ++
    (LAYER ++ SPLITS ++
      Set(ENDING, COMMENT) ++
      PAIR.keySet ++
      PAIR.values.toSet
      ).toList.sortBy(_.length)



}
