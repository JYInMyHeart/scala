package tpye
/**
  * @author xck
  */
class Lexer(var pos: Int)(var tokens: Array[String]) {
  def readStr(exp: String): XckType = {
    //    val lexer = Lexer(0)(List[String])
    pos = 0
    tokens = tokenizer(exp)
    readForm
  }

  def tokenizer(exp: String): Array[String] = {
    val pattern = "[\\s ,]*(~@|[\\[\\]{}()'`~@]|\"(?:[\\\\].|[^\\\\\"])*\"|;.*|[^\\s \\[\\]{}()'\"`~@,;]*)".r
    pattern.findAllMatchIn(exp).map {
      _.group(1)
    }
      .filter { s => s != "" && s(0) != ';' }
      .toArray
  }

  def peek: String = pos match {
    case x if x >= tokens.size => throw new NullPointerException
    case _ => tokens(pos)
  }

  def next: String =
    if (pos >= tokens.size)
      null
    else {
      pos += 1
      tokens(pos - 1)
    }

  def readList(value: XckList, c: Char, c1: Char):XckType = {
    val token = next
    def getList(token : String)(list:List[XckType]) : List[XckType] = {
      assert(token != null)
      token.charAt(0) match {
        case `c1` => list
        case _ => getList(peek)(list.::(readForm))
      }
    }
    val list:List[XckType] = List()
    val data = getList(token)(list).toArray
    val xckList = if (value.isList) new XckList(data) else new XckVector(data)
    xckList
  }

  def readAtom:XckType = {
    val reInt = "(^-?[0-9]+$)|(^-?[0-9][0-9.]*$)".r
    val reNil = "(^nil$)".r
    val reTrue = "(^true$)".r
    val reFalse = "(^false$)".r
    val reChar = "^\"(.*)\"$".r
    val reSkip = ":(.*)".r
    val re = "(^[^\"]*$)".r
    next match {
      case reInt(i) => new XckInt(i.toInt)
      case reNil(_) => new XckConstant("nil")
      case reTrue(_) => new XckConstant("true")
      case reFalse(_) => new XckConstant("false")
      case reChar(n) => new XckSymbol(n)
      case _ => throw new IllegalArgumentException
    }

  }

  def readForm : XckType = {
    val token = peek
    token.charAt(0) match {
      case '\'' =>
        next
        new XckList(new XckList(null).getList(new XckSymbol("quote"), readForm))
      case '`' =>
        next
        new XckList(new XckList(null).getList(new XckSymbol("quasiquote"), readForm))
      case '~' =>
        if (token == "~") {
          next
          new XckList(new XckList(null).getList(new XckSymbol("unquote"), readForm))
        }
        else {
          next
          new XckList(new XckList(null).getList(new XckSymbol("splice"), readForm))
        }
      case '@' =>
        next
        new XckList(new XckList(null).getList(new XckSymbol("deref"), readForm))
      case '(' =>
        readList(new XckList(null), '(', ')')
         //todo: break is not supported
      case ')' => throw new RuntimeException("unexpected ')'")
//      case '[' =>
//        form = readList(new Types.MalVector, '[', ']')
//        break //todo: break is not supported
//      case ']' =>
//        println("unexpected ']'")
      case _ =>
        readAtom
    }

  }

}



