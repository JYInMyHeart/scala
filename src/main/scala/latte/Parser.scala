package latte

import java.util.Stack

import latte.CompilerUtil._

case class Parser(root: ElementStartNode) {
  private var current: Node = root.linkNode
  private val parsedExps: Stack[Expression] = new Stack()
  private val binVarOps: Stack[String] = new Stack()
  private val unVarOps: Stack[String] = new Stack()
  private var usedVarNames: Set[String] = Set()
  private var modifiers: Set[Modifier] = Set()
  private var annos: Set[Anno] = Set()
  private var expectingStartNode = false
  private var isParsingTry = false
  private var isParsingMap = false
  private var isParsingOperatorLikeInvocation = false


  def addUsedVarNames(names: Set[String]): Unit =
    usedVarNames ++= names

  def annosIsEmpty(): Unit = {
    if (annos.nonEmpty) {
      var lineCol: LineCol = null
      annos.foreach { a =>
        if (lineCol == null || a.lineCol.line < lineCol.line)
          lineCol = a.lineCol
        else if (a.lineCol.line == lineCol.line
          && a.lineCol.column == lineCol.column)
          lineCol = a.lineCol
      }
      throw new Exception(s"annotations are not presented at correct position at $lineCol")
    }
  }

  def modifiersIsEmpty(): Unit = {
    if (modifiers.nonEmpty) {
      var lineCol: LineCol = null
      modifiers.foreach { a =>
        if (lineCol == null || a.lineCol.line < lineCol.line)
          lineCol = a.lineCol
        else if (a.lineCol.line == lineCol.line
          && a.lineCol.column == lineCol.column)
          lineCol = a.lineCol
      }
      throw new Exception(s"modifiers are not presented at correct position at $lineCol")
    }
  }

  def nextNode(canBeEnd: Boolean): Unit = {
    if (current == null) return
    val next = current.next
    if (next == null
      || (next.isInstanceOf[EndingNode]
      && next.asInstanceOf[EndingNode].nodeType == EndingNode.STRONG))
      if (!canBeEnd)
        throw new Exception(current.lineCol.toString)
    current = next
    next match {
      case node: EndingNode if node.nodeType == EndingNode.WEAK =>
        if (!canBeEnd)
          nextNode(false)
      case _ =>
    }
  }

  def parse: List[Statement] = {
    var list: List[Statement] = List()
    var enable = true
    while (enable) {
      if (isParsingMap) {
        annosIsEmpty()
        modifiersIsEmpty()
        if (current == null)
          enable = false
        else {
          parseExpression()
          val key = parsedExps.pop()
          nextNode(false)
          parseExpression()
          val value = parsedExps.pop()
          list :+= key
          list :+= value
          nextNode(true)
          binVarOps.clear()
        }
      } else {

      }
    }
    list
  }


  def isOneVariableOperatorPreMustCheckExps(content: String): Boolean =
    oneVarOperatorsPreMustCheckExps.contains(content)

  def isOneVariableOperatorPreWithoutCheckingExps(content: String): Boolean =
    oneVarOperatorsPreWithoutCheckingExps.contains(content)


  def parseExpression(): Unit = {
    if (current == null) return
    current match {
      case c: Element =>
        val content = c.content
        var doCheckParsedExps = true
        var enable = true
        while (enable) {
          if (doCheckParsedExps) {
            if (parsedExps.empty()) {
              if (isOneVariableOperatorPreMustCheckExps(content)) {
                annosIsEmpty()
                modifiersIsEmpty()
                parseOneVarPreOperation()
                return
              }
            }
            doCheckParsedExps = false
          } else {
            content match {
              case x if isNumber(x) =>
                annosIsEmpty()
                modifiersIsEmpty()
                val numberLiteral = NumberLiteral(content, current.lineCol)
                parsedExps.push(numberLiteral)
                nextNode(true)
                parseExpression()
              case x if isBoolean(x) =>
                annosIsEmpty()
                modifiersIsEmpty()
                val boolLiteral = BoolLiteral(content, current.lineCol)
                parsedExps.push(boolLiteral)
                nextNode(true)
                parseExpression()
              case x if isString(x) =>
                annosIsEmpty()
                modifiersIsEmpty()
                val stringLiteral = StringLiteral(content, current.lineCol)
                parsedExps.push(stringLiteral)
                nextNode(true)
                parseExpression()
              case "type" =>
                annosIsEmpty()
                modifiersIsEmpty()
                val lineCol = current.lineCol
                nextNode(false)
                val access = parseClsForTypeSpec()
                parsedExps.push(TypeOf(access, lineCol))
                parseExpression()
              case "null" =>
                annosIsEmpty()
                modifiersIsEmpty()
                parsedExps.push(Null(current.lineCol))
                nextNode(true)
                parseExpression()
              case "." =>
                annosIsEmpty()
                modifiersIsEmpty()
                parseAccess(true)
              case x if isOneVariableOperatorPreWithoutCheckingExps(x) =>
                annosIsEmpty()
                modifiersIsEmpty()
                parseOneVarPreOperation()
              case x if isOneVariableOperatorPost(x) =>
                annosIsEmpty()
                modifiersIsEmpty()
                parseOneVarPostOperation()
              case x if isTwoVariableOperator(x) =>
                annosIsEmpty()
                modifiersIsEmpty()
                parseTwoVarOperation()
              case x if isAssign(x) =>
                annosIsEmpty()
                modifiersIsEmpty()
                parseAssign()
              case ":" =>
                annosIsEmpty()
                modifiersIsEmpty()
                if (isParsingMap)
                  return
                else
                  parseTypeSpec()
              case "[" =>
                annosIsEmpty()
                modifiersIsEmpty()
                if (parsedExps.empty() || (isParsingMap && parsedExps.size() <= 1))
                  parseArrayExp()
                else
                  parseIndexAccess()
              case "{" =>
                annosIsEmpty()
                modifiersIsEmpty()
                parseMap()
              case "(" =>
                annosIsEmpty()
                modifiersIsEmpty()
                if (isLambda(current))
                  parseLambda()
                else {
                  nextNode(false)
                  current match {
                    case c: Element =>
                      expecting(")", c.previous, c)
                      if (!parsedExps.empty() && parsedExps.peek().isInstanceOf[Access]) {
                        val access = parsedExps.pop().asInstanceOf[Access]
                        val invocation = Invocation(access, List[Expression](), access.lineCol)
                        parsedExps.push(invocation)
                      } else
                        throw new Exception(s")${current.lineCol}")
                      nextNode(true)
                      parseExpression()
                    case c: ElementStartNode =>
                      val startNode = c
                      val statements: List[Statement] = parseElemStart(startNode, false, Set(), false, false)
                      if (statements.nonEmpty) {
                        if (!parsedExps.empty() && parsedExps.peek().isInstanceOf[Access]) {
                          val access = parsedExps.pop().asInstanceOf[Access]
                          val args = statements.map(_.asInstanceOf[Expression])
                          val invocation = Invocation(access, args, current.lineCol)
                          parsedExps.push(invocation)
                        } else {
                          statements.size match {
                            case 1 =>
                              statements.head match {
                                case e: Expression =>
                                  parsedExps.push(e)
                                case r: Return =>
                                  val procedure = Procedure(statements, startNode.lineCol)
                                  parsedExps.push(procedure)
                                case _ =>
                                  throw new Exception(s"return statement in closure ${statements.head.toString}")
                              }
                            case _ =>
                              val procedure = Procedure(statements, startNode.lineCol)
                              parsedExps.push(procedure)
                          }
                        }
                      } else
                        throw new Exception(s"arguments ${startNode.toString}")
                      nextNode(false)
                      expecting(")", startNode, current)
                      nextNode(true)
                      parseExpression()
                  }

                }

              case "as" =>
                annosIsEmpty()
                if (parsedExps.empty())
                  throw new Exception(s"unexpected expression as ${current.lineCol}")
                else {
                  val lineCol = current.lineCol
                  val exp = parsedExps.pop()
                  nextNode(true)
                  val access = parseClsForTypeSpec()
                  val asType = AsType(exp, access, lineCol)
                  parsedExps.push(asType)
                }
              case "undefined" =>
                annosIsEmpty()
                parsedExps.push(UndefinedExp(current.lineCol))
                nextNode(true)
                parseExpression()
              case _ =>
                current match {
                  case x: Element if isPackage(x) =>
                    annosIsEmpty()
                    modifiersIsEmpty()
                    parsePackage(true)
                  case x: Element if x.isValidName =>
                    if (parsedExps.empty())
                      parseVar()
                    else
                      parseOperatorLikeInvocation()
                  case _ =>
                    throw new Exception(s"unknown token $content ${current.lineCol}")
                }
            }
          }
        }
      case _: ElementStartNode =>
        if (!expectingStartNode)
          throw new Exception(s"unexpected new layer ${current.lineCol}")
      case _ =>
    }
  }

  def parseOperatorLikeInvocation(): Unit = {

  }

  def parseVar(): Unit = {

  }

  def parsePackage(boolean: Boolean): Unit = {

  }


  def parseElemStart(startNode: ElementStartNode,
                     bool: Boolean,
                     set: Set[Nothing],
                     bool1: Boolean,
                     bool2: Boolean): List[Statement] = {
    List()
  }

  def parseArrayExp(): Unit = {

  }

  def parseIndexAccess(): Unit = {

  }

  def parseLambda(): Unit = {

  }

  def parseMap(): Unit = {

  }

  def parseTypeSpec(): Unit = {

  }

  def parseAssign(): Unit = {

  }

  def parseTwoVarOperation(): Unit = {

  }

  def parseOneVarPostOperation(): Unit = {

  }

  def parseAccess(parseExp: Boolean): Unit = {

  }

  def parseClsForTypeSpec(): Access = {
    null
  }


  def parseOneVarPreOperation(): Unit = {
    //    val opNode = current.asInstanceOf[Element]
    //    val op = opNode.content
    //    unVarOps.push(op)
    //    val exp = nextExp(false)


  }

}


