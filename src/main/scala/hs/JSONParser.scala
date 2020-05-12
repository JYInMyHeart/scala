package hs
import hs.JSONParser.JTokenType.JTokenType

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object JSONParser {

  def main(args: Array[String]): Unit = {
    val json =
      "{\n  \"id\": \"123\",\n  \"ida\": {\n    \"levela\": {\n      \"levelaa\": \"1\"\n    },\n    \"levelb\": {\n      \"levelab\": \"1\"\n    }\n  },\n  \"data\": {\n    \"dataFormA\": [\n      {\n        \"dataName\": {\n          \"levelab\": \"1\"\n        },\n        \"dataType\": \"1\",\n        \"dataValue\": \"1\"\n      },\n      {\n        \"dataName\": \"2\",\n        \"dataType\": \"2\",\n        \"dataValue\": \"2\"\n      }\n    ],\n    \"dataFormB\": [\n      {\n        \"dataName\": \"2\",\n        \"dataType\": \"2\",\n        \"dataValue\": \"2\"\n      }\n    ]\n  }\n}";
    val jsonText = json.replace("\"", "")
    val lexer = new Lexer(jsonText, 0)
    val parser = new Parser(lexer.lexer())
    val v = parser.parse()
    printJSON(v)
  }

  def printJSON(jValue: JValue): Unit = {
    def printObject(value: JValue): List[String] = {
      value match {
        case j: JObject =>
          j.value.flatMap {
            case (k, v) =>
              printObject(v).map(
                if (v.isLowLevel) s"$k--->" + _
                else if (v.isInstanceOf[JArray]) s"$k" + _
                else s"$k." + _
              )
          }.toList
        case j: JArray =>
          j.value.zipWithIndex.flatMap {
            case (v, i) =>
              printObject(v).map(s"[$i]." + _)
          }.toList
        case j @ (_: JValue) => List(s"${j.getValue}\n")

      }
    }
    println(printObject(jValue))

  }

  trait JValue {
    def getValue: Any
    def isLowLevel: Boolean
  }

  case class JString(value: String) extends JValue {
    override def getValue: Any = value
    override def isLowLevel: Boolean = true
  }

  case class JNumber(value: Int) extends JValue {
    override def getValue: Any = value
    override def isLowLevel: Boolean = true
  }
  case class JObject(value: mutable.LinkedHashMap[String, JValue])
      extends JValue {
    override def getValue: Any = value
    override def isLowLevel: Boolean = false
  }
  case class JArray(value: ListBuffer[JValue]) extends JValue {
    override def getValue: Any = value
    override def isLowLevel: Boolean = false
  }
  case class JBoolean(value: Boolean) extends JValue {
    override def getValue: Any = value
    override def isLowLevel: Boolean = true
  }
  case class JEmpty(value: Null) extends JValue {
    override def getValue: Any = value
    override def isLowLevel: Boolean = true
  }

  sealed class Parser(jsonToken: List[Token]) {
    private[this] var position: Int = _
    def parse(): JValue = {
      parseStatement()
    }

    private def nextToken: Token = {
      val currentToken = current
      position += 1
      currentToken
    }
    def eat(t: JTokenType): Token = current.tokenType match {
      case t => nextToken
      case _ => throw new IllegalArgumentException("Unexpected character!")
    }

    def peek(offset: Int): Token = {
      val index = offset + position
      if (index >= jsonToken.size) {
        return jsonToken.last
      }
      jsonToken(index)
    }

    def current: Token = peek(0)

    def parseStatement(): JValue = {
      current.tokenType match {
        case JTokenType.lm   => parseArray()
        case JTokenType.lb   => parseObject()
        case JTokenType.str  => parseStr()
        case JTokenType.num  => parseNum()
        case JTokenType.tre  => parseBool(true)
        case JTokenType.fals => parseBool(false)
        case _               => throw new IllegalArgumentException("unknown token")
      }
    }

    def parseBool(value: Boolean): JBoolean = {
      val bool = JBoolean(value)
      nextToken
      bool
    }
    def parseNum(): JNumber = {
      val num = JNumber(current.value.toInt)
      nextToken
      num
    }

    def parseStr(): JString = {
      val str = JString(current.value)
      nextToken
      str
    }
    //{}
    def parseObject(): JObject = {
      eat(JTokenType.lb)
      val value = parseMembers()
      eat(JTokenType.rb)
      JObject(value)
    }

    def parseMembers(): mutable.LinkedHashMap[String, JValue] = {
      val map = mutable.LinkedHashMap[String, JValue]()
      val first = parseMember()
      map += first
      while (current.tokenType != JTokenType.rb) {
        eat(JTokenType.comma)
        map += parseMember()
      }
      map
    }

    def parseMember(): (String, JValue) = {
      val key = eat(JTokenType.str).value
      eat(JTokenType.colon)
      val value = parseStatement()
      (key -> value)
    }

    def parseArray(): JArray = {
      val array = ListBuffer[JValue]()
      eat(JTokenType.lm)
      val fisrt = parseStatement()
      array += fisrt
      while (current.tokenType != JTokenType.rm) {
        eat(JTokenType.comma)
        array += parseStatement()
      }
      eat(JTokenType.rm)
      JArray(array)
    }

  }

  object JTokenType extends Enumeration {
    type JTokenType = Value
    val str = Value
    val num = Value
    val lb = Value
    val rb = Value
    val lm = Value
    val rm = Value
    val comma = Value
    val colon = Value

    val tre = Value
    val fals = Value
  }
  case class Token(value: String, tokenType: JTokenType)
  class Lexer(jsonText: String, var offset: Int) {
    def ch: Char = jsonText.charAt(offset)
    def next(): Unit = offset += 1
    def lexer(): List[Token] = {
      val list = ListBuffer[Token]()
      while (offset < jsonText.length) {
        ch match {
          case '\n' => next()
          case ' '  => next()
          case '\t' => next()
          case '\r' => next()
          case '{' =>
            list.+=(Token("{", JTokenType.lb))
            next()
          case '}' =>
            list.+=(Token("}", JTokenType.rb))
            next()
          case '[' =>
            list.+=(Token("[", JTokenType.lm))
            next()
          case ']' =>
            list.+=(Token("]", JTokenType.rm))
            next()
          case ':' =>
            list.+=(Token(":", JTokenType.colon))
            next()
          case ',' =>
            list.+=(Token(",", JTokenType.comma))
            next()
          case x if Character.isLetter(x) =>
            val str = lexerString()
            str match {
              case "true"  => list.+=(Token(str, JTokenType.tre))
              case "false" => list.+=(Token(str, JTokenType.fals))
              case _       => list.+=(Token(str, JTokenType.str))
            }
          case x if Character.isDigit(x) =>
            val num = lexerNum()
            list.+=(Token(num, JTokenType.num))
          case _ =>
            throw new IllegalArgumentException("unknown json character")
        }
      }
      list.toList
    }

    def lexerString(): String = {
      lexerCh(Character.isLetter)
    }

    def lexerNum(): String = {
      lexerCh(Character.isDigit)
    }

    def lexerCh(f: Char => Boolean): String = {
      var index = 0
      while (f(jsonText.charAt(offset + index))) {
        index += 1
      }
      val res = jsonText.substring(offset, offset + index)
      offset += index
      res
    }
  }

}
