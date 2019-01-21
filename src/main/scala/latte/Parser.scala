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
      throw new Exception(
        s"annotations are not presented at correct position at $lineCol")
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
      throw new Exception(
        s"modifiers are not presented at correct position at $lineCol")
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
      } else {}
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
                if (parsedExps
                      .empty() || (isParsingMap && parsedExps.size() <= 1))
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
                      if (!parsedExps.empty() && parsedExps
                            .peek()
                            .isInstanceOf[Access]) {
                        val access = parsedExps.pop().asInstanceOf[Access]
                        val invocation =
                          Invocation(access, List[Expression](), access.lineCol)
                        parsedExps.push(invocation)
                      } else
                        throw new Exception(s")${current.lineCol}")
                      nextNode(true)
                      parseExpression()
                    case c: ElementStartNode =>
                      val startNode = c
                      val statements: List[Statement] =
                        parseElemStart(startNode, false, Set(), false, false)
                      if (statements.nonEmpty) {
                        if (!parsedExps.empty() && parsedExps
                              .peek()
                              .isInstanceOf[Access]) {
                          val access = parsedExps.pop().asInstanceOf[Access]
                          val args = statements.map(_.asInstanceOf[Expression])
                          val invocation =
                            Invocation(access, args, current.lineCol)
                          parsedExps.push(invocation)
                        } else {
                          statements.size match {
                            case 1 =>
                              statements.head match {
                                case e: Expression =>
                                  parsedExps.push(e)
                                case r: Return =>
                                  val procedure =
                                    Procedure(statements, startNode.lineCol)
                                  parsedExps.push(procedure)
                                case _ =>
                                  throw new Exception(
                                    s"return statement in closure ${statements.head.toString}")
                              }
                            case _ =>
                              val procedure =
                                Procedure(statements, startNode.lineCol)
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
                  throw new Exception(
                    s"unexpected expression as ${current.lineCol}")
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
                    throw new Exception(
                      s"unknown token $content ${current.lineCol}")
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
    if (isParsingOperatorLikeInvocation) return
    assert(!parsedExps.empty())
    val expr = parsedExps.pop()
    val op = current.asInstanceOf[Element].content
    val opLineCol = current.lineCol
    current.next match {
      case e: Element =>
        if (!isParsingMap || e.next.asInstanceOf[Element].content != ":") {
          if (!binVarOps.empty()) {
            val lastOp = binVarOps.pop()
            if (twoVarHigherOrEqual(lastOp, op)) {
              parsedExps.push(expr)
              return
            }
            binVarOps.push(lastOp)
          }
          nextNode(true)
          binVarOps.push(op)
          var opArgs = List[Expression]()
          opArgs :+= getExp(false)
          while (current.isInstanceOf[EndingNode]
                 && current
                   .asInstanceOf[EndingNode]
                   .nodeType == EndingNode.STRONG) {
            val tmp = new Stack[String]()
            while (!binVarOps.empty()) tmp.push(binVarOps.pop())
            nextNode(false)
            isParsingOperatorLikeInvocation = true
            opArgs :+= getExp(false)
            isParsingOperatorLikeInvocation = false
            while (!tmp.empty()) binVarOps.push(tmp.pop())
          }
          val invocation = Invocation(
            Access(expr, op, opLineCol),
            opArgs,
            opLineCol
          )
          parsedExps.push(invocation)
        }
      case _ =>
        if (!binVarOps.empty()) {
          binVarOps.pop()
          parsedExps.push(expr)
          return
        }
        nextNode(true)
        val invocation = Invocation(
          Access(expr, op, opLineCol),
          List(),
          opLineCol
        )
        parsedExps.push(invocation)
    }
    parseExpression()
  }

  def getExp(expectingStartNode: Boolean): Expression = {
    if (expectingStartNode)
      this.expectingStartNode = true
    parseExpression()
    if (expectingStartNode)
      this.expectingStartNode = false
    parsedExps.pop()
  }

  def nextExp(expectingStartNode: Boolean): Expression = {
    nextNode(false)
    getExp(expectingStartNode)
  }

  def parseVar(): Unit = {
    val content = current.asInstanceOf[Element].content
    if (modifiers.nonEmpty || annos.nonEmpty) {
      if (usedVarNames.contains(content)) {
        throw DuplicateVariableNameException(content, current.lineCol)
      }
      val vdef =
        VariableDef(content, modifiers, null, null, annos, current.lineCol)
      annos = Set()
      modifiers = Set()
      usedVarNames += content
      parsedExps.push(vdef)
    } else {
      val access = Access(null, content, current.lineCol)
      parsedExps.push(access)
    }
    nextNode(true)
    parseExpression()
  }

  def parseModifier() = {
    val elem = current.asInstanceOf[Element]
    val modifier = elem.content
    if (modifierIsCompatible(modifier, modifiers))
      modifiers += Modifier(modifier, current.lineCol)
    else
      throw UnexpectedTokenException("valid modifier", modifier, elem.lineCol)
  }

  def parsePackage(parseExp: Boolean): Unit = {
    val sb = new StringBuilder()
    var isName = true
    val lineCol = current.lineCol
    while (current != null
           && (current.isInstanceOf[Element]
           && current.asInstanceOf[Element].content == "::")
           || current.asInstanceOf[Element].isValidName) {
      val s = current.asInstanceOf[Element].content
      if (!isName && s != "::")
        throw UnexpectedTokenException("::",
                                       s,
                                       current.asInstanceOf[Element].lineCol)
      isName = !isName
      sb.append(s)
      nextNode(true)
    }
    val str = sb.toString()
    val lastIndex = str.lastIndexOf("::")
    val pkg = PackageRef(str.substring(0, lastIndex), lineCol)
    val cls = str.substring(lastIndex + 2)
    val access = Access(pkg, cls, lineCol)
    parsedExps.push(access)
    if (parseExp)
      parseExpression()
  }

  def parseElemStart(startNode: ElementStartNode,
                     addUsedNames: Boolean,
                     names: Set[String],
                     parseMap: Boolean,
                     parseTry: Boolean): List[Statement] = {
    val parser = Parser(startNode)
    if (addUsedNames) {
      parser.addUsedVarNames(usedVarNames)
      parser.addUsedVarNames(names)
    }
    parser.isParsingMap = parseMap
    parser.isParsingTry = parseTry
    parser.parse
  }

  def parseArrayExp(): Unit = {
    val lineCol = current.lineCol
    nextNode(false)
    current match {
      case e: Element => {
        expecting("]", e.previous, e)
        parsedExps.push(ArrayExp(List(), lineCol))
        nextNode(true)
      }
      case _ => {
        expecting("]", if (current.next == null) null else current.next.next)
        val stmts: List[Statement] = parseElemStart(
          current.asInstanceOf[ElementStartNode],
          true,
          Set(),
          false,
          false)
        var exps: List[Expression] = List()
        stmts.foreach(exps :+= _.asInstanceOf[Expression])
        parsedExps.push(ArrayExp(exps, lineCol))
        nextNode(false)
        nextNode(true)
      }
    }
    parseExpression()
  }

  def parseIndexAccess(): Unit = {}

  def parseLambda(): Unit = {}

  def parseMap(): Unit = {
    val lineCol = current.lineCol
    nextNode(false)
    current match {
      case e: Element => {
        expecting("}", e.previous, e)
        parsedExps.push(MapExp(null, lineCol))
        nextNode(true)
      }
      case _ => {
        expecting("}",
                  current,
                  if (current.next == null) null else current.next.next)
        parsedExps.push(parseExpMap(current.asInstanceOf[ElementStartNode]))
        nextNode(false)
        nextNode(true)
      }
      parseExpression()
    }
  }

  def parseExpMap(startNode: ElementStartNode): MapExp = {
    null
  }

  def parseTypeSpec(): Unit = {
    val lineCol = current.lineCol
    assert(!parsedExps.empty())
    val expr = parsedExps.pop()
    expr match {
      case v: Access =>
        if (v.expression != null)
          throw DuplicateVariableNameException(v.name, v.lineCol)
        if (usedVarNames.contains(v.name))
          throw UnexpectedTokenException("variable", v.toString, v.lineCol)
        nextNode(false)
        current match {
          case _: Element =>
            val a = parseClsForTypeSpec()
            v.asInstanceOf[VariableDef].vType = a
          case _ =>
            throw UnexpectedTokenException(
              "type",
              current.toString,
              if (current == null) lineCol else current.lineCol)
        }
        parsedExps.push(expr)
        parseExpression()
    }

  }

  def parseAssign(): Unit = {
    val op = current.asInstanceOf[Element].content
    assert(!parsedExps.empty())
    val expr = parsedExps.pop()
    val lineCol = current.lineCol
    expr match {
      case e: Access =>
        if (e.expression == null && !usedVarNames.contains(e.name)) {
          val variableDef =
            VariableDef(e.name, modifiers, null, null, annos, e.lineCol)
          annos = Set()
          modifiers = Set()
          usedVarNames += e.name
          val expression = nextExp(false)
          variableDef.init = expression
          parsedExps.push(variableDef)
        } else {
          val expression = nextExp(false)
          val assignment = Assignment(e, op, expression, lineCol)
          parsedExps.push(assignment)
        }
      case e: Index =>
        val expression = nextExp(false)
        val assignment =
          Assignment(Access(e, null, e.lineCol), op, expression, lineCol)
        parsedExps.push(assignment)
      case v: VariableDef =>
        val expression = nextExp(false)
        v.init = expression
        parsedExps.push(v)
      case _ =>
        throw UnexpectedTokenException("variable",
                                       expr.toString,
                                       current.lineCol)
    }
    parseExpression()
  }

  def parseTwoVarOperation(): Unit = {
    val opNode = current.asInstanceOf[Element]
    val op = opNode.content
    val lineCol = current.lineCol
    assert(!parsedExps.empty())
    val exp = parsedExps.pop()

    if (!unVarOps.empty()) {
      parsedExps.push(exp)
      return
    }
    if (!binVarOps.empty() && twoVarHigherOrEqual(binVarOps.peek(), op)) {
      parsedExps.push(exp)
      binVarOps.pop()
      return
    }
    binVarOps.push(op)
    val exp1 = nextExp(false)
    val tvo = TwoVariableOperation(op, exp, exp1, lineCol)
    parsedExps.push(tvo)
    parseExpression()
  }

  def parseAccess(parseExp: Boolean): Unit = {
    val lineCol = current.lineCol
    assert(!parsedExps.empty())
    val exp = parsedExps.pop()
    nextNode(false)
    current match {
      case e: Element =>
        val name = e.content
        if (!e.isValidName)
          throw UnexpectedTokenException("valid name", name, current.lineCol)
        val access = Access(exp, name, lineCol)
        parsedExps.push(access)
        nextNode(true)
        if (parseExp)
          parseExpression()
      case _ =>
        throw UnexpectedTokenException("valid name",
                                       current.toString(),
                                       current.lineCol)
    }
  }

  def parseClsForTypeSpec(): Access = {
    var access: Access = null
    var arrayDepth = 0
    while (current.asInstanceOf[Element].content == "[") {
      nextNode(false)
      expecting("]", current.previous, current)
      nextNode(false)
      arrayDepth += 1
    }
    if (isPackage(current.asInstanceOf[Element])) {
      parsePackage(false)
      while (current.isInstanceOf[Element]
             && current.asInstanceOf[Element].content == ".") {
        parseAccess(false)
      }
      access = parsedExps.pop().asInstanceOf[Access]
    } else if (current.asInstanceOf[Element].isValidName
               || isPrimitive(current.asInstanceOf[Element].content)) {
      val accessTmp =
        Access(null, current.asInstanceOf[Element].content, current.lineCol)
      parsedExps.push(accessTmp)
      nextNode(true)
      while (current.isInstanceOf[Element]
             && current.asInstanceOf[Element].content == ".") {
        parseAccess(false)
      }
      access = parsedExps.pop().asInstanceOf[Access]
    } else
      throw new Exception(
        s"unexpected type ${current.asInstanceOf[Element].content} at ${current.lineCol}")
    for (_ <- 0 until arrayDepth)
      access = Access(access, "[]", access.lineCol)
    access
  }

  def parseOneVarPreOperation(): Unit = {
    val opNode = current.asInstanceOf[Element]
    val op = opNode.content
    unVarOps.push(op)
    val exp = nextExp(false)
    val uovo = UnaryOneVariableOperation(op, exp, opNode.lineCol)
    parsedExps.push(uovo)
    unVarOps.pop()
    parseExpression()
  }

  def parseOneVarPostOperation(): Unit = {
    val opNode = current.asInstanceOf[Element]
    val op = opNode.content
    assert(!parsedExps.empty())
    val e = parsedExps.pop()
    val ovo = OneVariableOperation(op, e, opNode.lineCol)
    parsedExps.push(ovo)
    nextNode(true)
    parseExpression()
  }
}
