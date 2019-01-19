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

  val keys: Set[String] = Set(
    "is", "bool", "yes", "no", "type", "as", "undefined"
  )

  def isNumber(str:String) :Boolean =
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


}
