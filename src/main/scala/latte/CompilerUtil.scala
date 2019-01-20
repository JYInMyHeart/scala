package latte

object CompilerUtil {

  val javaKeys: Set[String] = Set(
    "abstract", "assert", "boolean", "break", "byte", "case",
    "catch", "char", "class", "const", "continue", "default",
    "do", "double", "else", "enum", "extends", "final", "finally",
    "float", "for", "if", "implements", "import", "instanceof",
    "int", "interface", "long", "native", "new", "package",
    "private", "protected", "public", "return", "short", "static",
    "strictfp", "null"
  )

  val modifiers: Set[String] = Set(
    "pub", "pro", "pri", "pkg",
    "abs", "val", "native", "sync", "transient", "volatile", "strictfp",
    "data"
  )

  val accessModifiers: Set[String] = Set(
    "pub", "pro", "pri", "pkg"
  )

  val keys: Set[String] = Set(
    "is", "bool", "yes", "no", "type", "as", "undefined"
  ) ++ modifiers ++ javaKeys

  val oneVarOperatorsPost: Set[String] = Set("++", "--")

  val binOpPriority: Array[Array[String]] = Array(
    Array("..", ".:"),
    Array("^^"),
    Array("*", "/", "%"),
    Array("+", "-"),
    Array("<<", ">>", ">>>"),
    Array(">", "<", ">=", "<="),
    Array("==", "!=", "===", "!==", "=:=", "!:=", "is", "not", "in"),
    Array("&"),
    Array("^"),
    Array("|"),
    Array("&&", "and"),
    Array("||", "or")
  )

  val twoVarOperators: Set[String] = binOpPriority.foldLeft(List[String]())(_ ::: _.toList).toSet

  def isNumber(str: String): Boolean =
    str.forall(Character.isDigit)

  def isValidNameStartChar(c: Char): Boolean =
    Character.isLetter(c) || c == '$' || c == '_'

  def isValidNameChar(c: Char): Boolean =
    isValidNameStartChar(c) || Character.isDigit(c)

  def isJavaValidName(str: String): Boolean = {
    str match {
      case x if x.isEmpty => false
      case x if javaKeys.contains(x) => false
      case x if isValidNameStartChar(x.charAt(0)) =>
        for (i <- 1 until x.length) {
          if (!isValidNameChar(x.charAt(i)))
            return false
        }
        true
      case _ => true
    }
  }

  def isValidName(str: String): Boolean = {
    if (str.startsWith("`") && str.endsWith("`"))
      return isJavaValidName(str.substring(1, str.length - 1))
    isJavaValidName(str) && !keys.contains(str)
  }

  def isBoolean(str: String): Boolean =
    str == "true" || str == "false"


  def isString(str: String): Boolean =
    (str.startsWith("\"") && str.endsWith("\"")) || (str.startsWith("\'") && str.endsWith("\'"))


  def isOneVariableOperatorPost(str: String): Boolean =
    oneVarOperatorsPost.contains(str)

  def isTwoVariableOperator(str: String): Boolean =
    twoVarOperators.contains(str)

  def getNextNode(element: Node): Node = {
    if (element == null) return null
    if (element.next.isInstanceOf[EndingNode])
      getNextNode(element.next)
    else
      element.next
  }

  def isLambda(elem: Node): Boolean = {
    elem match {
      case e: Element =>
        if (e.content == "(") {
          var n = getNextNode(e)
          n match {
            case x: Element =>
              if (x.content == ")") {
                n = getNextNode(n)
                n match {
                  case y: Element =>
                    if (y.content == "->")
                      return true
                  case _ =>
                }
              }
            case _: ElementStartNode =>
              n = getNextNode(n)
          }
        }
    }
    false
  }


  def isAssign(str: String): Boolean =
    str == "=" || str == "/=" || str == "*=" || str == "+=" || str == "-=" || str == "%="


  def expecting(token: String, previous: Node, got: Node): Unit = {
    got match {
      case x: Element if !x.content.endsWith(token) =>
      case _ =>
        throw new Exception(s"${previous.lineCol}")
    }
  }

  def isPackage(element: Element): Boolean = {
    if (isValidName(element.content) && element.hasNext) {
      element.next match {
        case n: Element =>
          if (n.content == "::" && n.hasNext) {
            n.next match {
              case nn: Element =>
                return isValidName(nn.content)
              case _ =>
                return false
            }
          }
        case _ =>
          return false
      }
    }
    false
  }


}
