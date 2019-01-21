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
      } else {
        val stmt = parseStatement()
        if (current == null && stmt == null)
          return list
        else if (!parsedExps.empty()) {
          val sb = new StringBuilder()
          parsedExps.forEach { x =>
            sb.append(s"${x.toString} at ${x.lineCol.fileName}(${x.lineCol.line},${x.lineCol.column})\n")
          }
          val msg = s"parsed expression stack should be empty, but got\n $sb and got statement $stmt"
          throw new LtBug(msg)
        }
        assert(unVarOps.empty())
        binVarOps.clear()
        if (stmt != null)
          list :+= stmt
        nextNode(true)
      }
    }
    list
  }


  def parseStatement(): Statement = {
    if (current == null) return null
    isParsingTry match {
      case true => {
        annosIsEmpty()
        modifiersIsEmpty()
        val lineCol = current.lineCol
        var exceptionTypes = List[Access]()
        while (current.isInstanceOf[Element]) {
          val exp = getExp(true)
          exp match {
            case access: Access =>
              exceptionTypes :+= access
            case _ =>
              throw UnexpectedTokenException("exception type", exp.toString, exp.lineCol)
          }
          current match {
            case node: EndingNode if node.nodeType == EndingNode.STRONG => nextNode(true)
            case _ =>
          }
        }
        current match {
          case node: ElementStartNode =>
            Catch(
              exceptionTypes,
              parseElemStart(
                node,
                addUsedNames = true,
                Set(),
                parseMap = false,
                parseTry = false),
              lineCol)
          case _ => throw new UnexpectedTokenException(current.toString(), current.lineCol)
        }
      }
      case false => {
        current match {
          case e: Element => {
            val content = e.content
            content match {
              case c if isModifier(c) && e.isValidName =>
                parseModifier()
                null
              case "if" =>
                annosIsEmpty()
                modifiersIsEmpty()
                parseIf()
              case "for" =>
                annosIsEmpty()
                modifiersIsEmpty()
                parseFor()
              case "do" =>
                annosIsEmpty()
                modifiersIsEmpty()
                parseDo()
              case "while" =>
                annosIsEmpty()
                modifiersIsEmpty()
                parseWhile()
              case "static" => {
                annosIsEmpty()
                modifiersIsEmpty()

                val lineCol = e.lineCol
                e.next match {
                  case ee: ElementStartNode =>
                    nextNode(false)
                    StaticScope(
                      parseElemStart(
                        ee,
                        addUsedNames = false,
                        Set(),
                        parseMap = false,
                        parseTry = false
                      ),
                      lineCol
                    )
                  case ee: Element =>
                    nextNode(false)
                    val stmt = parseStatement()
                    if (stmt == null)
                      throw UnexpectedTokenException("a valid statement", ee.toString, ee.lineCol)
                    StaticScope(List(stmt), lineCol)
                  case _ =>
                    null
                }
              }
              case "class" =>
                parseClass()
              case "interface" =>
                parseInterface()
              case "try" =>
                annosIsEmpty()
                modifiersIsEmpty()
                parseTry()
              case "throw" =>
                annosIsEmpty()
                modifiersIsEmpty()
                parseThrow()
              case "@" =>
                modifiersIsEmpty()
                parseAnno()
              case "<" =>
                annosIsEmpty()
                modifiersIsEmpty()
                val lineCol = e.lineCol
                e.next match {
                  case _: Element =>
                    Return(null, lineCol)
                  case _ =>
                    Return(nextExp(false), lineCol)
                }
              case "#" =>
                modifiersIsEmpty()
                parsePkgDeclare()
              case "#>" =>
                annosIsEmpty()
                modifiersIsEmpty()
                parsePkgImport()
              case _ => {
                val defMethodType = checkMethodDef(e)
                defMethodType match {
                  case METHOD_DEF_TYPE =>
                    parseMethodDefType()
                  case METHOD_DEF_EMPTY =>
                    parseMethodDefEmpty()
                  case METHOD_DEF_NORMAL =>
                    parseMethodDefNormal()
                  case METHOD_DEF_ONE_STMT =>
                    parseMethodDefOneStmt()
                  case _ =>
                    while (true) {
                      parseExpression()
                      if (e == null) {
                        if (parsedExps.empty()) return null
                        return parsedExps.pop()
                      }
                    }
                    null
                }
              }
            }
          }
          case _ =>
            nextNode(true)
            parseStatement()
        }
      }
    }


  }

  def parseIf(): IfStatement = {
    val lineCol = current.lineCol
    var pairs = List[IfPair]()
    var isLast = false
    var enable = true
    while (enable && (current.isInstanceOf[Element]
      || current.isInstanceOf[EndingNode])) {
      val ifPairLineCol = current.lineCol
      var condition: Expression = null
      var list: List[Statement] = List()
      current match {
        case e: EndingNode => {
          e.next match {
            case ee: Element => {
              val content = ee.content
              if (content == "elseif" || content == "else")
                nextNode(false)
              else {
                enable = false
              }
            }
            case _ =>
          }
        }
        case e: Element => {
          if (e.content == "else")
            nextNode(true)
          else
            nextNode(false)
          if (e.content == "if" || e.content == "elseif") {
            if (isLast)
              throw new SyntaxException(
                s"if-else had already reached else but got ${e.content} instead",
                current.lineCol)
            condition = getExp(true)
          }
        }
        case e: ElementStartNode => {
          list = parseElemStart(
            e,
            addUsedNames = true,
            Set(),
            parseMap = false,
            parseTry = false
          )
        }
        case _ =>
      }
      if (condition == null)
        isLast = true
      val pair = IfPair(
        condition,
        if (list == null) List() else list,
        ifPairLineCol)
      pairs :+= pair
      nextNode(true)
      binVarOps.clear()
    }
    if (current != null)
      current = current.next
    IfStatement(pairs, lineCol)
  }

  def parseFor(): ForStatement = {
    val lineCol = current.lineCol
    nextNode(false)
    val varElem = current.asInstanceOf[Element]
    val varName = varElem.content
    if (!varElem.isValidName)
      throw UnexpectedTokenException("valid variable name", varName, current.lineCol)
    if (usedVarNames.contains(varName))
      throw DuplicateVariableNameException(varName, current.lineCol)
    nextNode(false)
    expecting("in", current.previous, current)
    val exp = nextExp(true)
    var statements: List[Statement] = List()
    current match {
      case e: ElementStartNode => {
        val processor = Parser(e)
        var set = usedVarNames
        set += varName
        processor.addUsedVarNames(set)
        statements = processor.parse
      }
      case _ =>
    }
    ForStatement(
      varName,
      exp,
      statements,
      lineCol)
  }

  def parseDo(): WhileStatement = {
    val lineCol = current.lineCol
    nextNode(true)
    current match {
      case e: ElementStartNode => {
        var statements = parseElemStart(
          e,
          addUsedNames = true,
          Set(),
          parseMap = false,
          parseTry = false
        )
        nextNode(false)
        expecting("while", current.previous, current)
        val condition = nextExp(true)
        WhileStatement(condition, statements, true, lineCol)
      }
      case _ => {
        throw UnexpectedTokenException("while", current.toString, current.lineCol)
      }
    }
  }

  def parseWhile(): WhileStatement = {
    val lineCol = current.lineCol
    val condition = nextExp(true)
    current match {
      case e: ElementStartNode =>
        WhileStatement(
          condition,
          parseElemStart(
            e,
            addUsedNames = true,
            Set(),
            parseMap = false,
            parseTry = false
          ),
          doWhile = false,
          lineCol)
      case _ =>
        throw UnexpectedTokenException("while body", current.toString, current.lineCol)
    }
  }

  def parseClass(): ClassStatement = {
    //todo
    null
  }

  def parseInterface(): Statement = {
    //todo
    null
  }

  def parseTry(): Statement = {
    //todo
    null
  }

  def parseThrow(): Statement = {
    //todo
    null
  }

  def parseAnno(): Statement = {
    //todo
    null
  }

  def parsePkgDeclare(): Statement = {
    //todo
    null
  }

  def parsePkgImport(): Statement = {
    val lineCol = current.lineCol
    var importDetails = List[ImportDetail]()
    nextNode(false)
    current match {
      case e: ElementStartNode => {
        val processor = Parser(e)
        val statements = processor.parse
        statements.foreach {
          case s: Access => {
            var detail: ImportDetail = null
            if (s.name == "_") {
              s.expression match {
                case p: PackageRef =>
                  detail = ImportDetail(p, null, true)
                case _ =>
                  detail = ImportDetail(null, s.expression.asInstanceOf[Access], true)
              }
            } else
              detail = ImportDetail(null, s, false)
            importDetails :+= detail
          }
          case _ =>
            throw UnexpectedTokenException("import statements", current.toString, current.lineCol)
        }
      }
      case _ =>
    }
    Import(importDetails, lineCol)
  }

  def checkMethodDef(e: Element):Int = ???

  def parseMethodDefType(): Statement = ???

  def parseMethodDefEmpty(): Statement = ???

  def parseMethodDefNormal(): Statement = ???

  def parseMethodDefOneStmt(): Statement = ???

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
                        parseElemStart(startNode, addUsedNames = false, Set(), parseMap = false, parseTry = false)
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
        expecting("]", if (current.next == null) null else current.next.next, current)
        val stmts: List[Statement] = parseElemStart(
          current.asInstanceOf[ElementStartNode],
          true,
          Set(),
          parseMap = false,
          parseTry = false)
        var exps: List[Expression] = List()
        stmts.foreach(exps :+= _.asInstanceOf[Expression])
        parsedExps.push(ArrayExp(exps, lineCol))
        nextNode(false)
        nextNode(true)
      }
    }
    parseExpression()
  }

  def parseIndexAccess(): Unit = {
    val exp = parsedExps.pop()
    nextNode(false)
    current match {
      case e: Element =>
        expecting("]", e.previous, e)
        parsedExps.push(Index(exp, List(), e.lineCol))
      case _ =>
        expecting("]", current, if (current.next == null) null else current.next.next)
        var exps = List[Expression]()
        val stmts = parseElemStart(
          current.asInstanceOf[ElementStartNode],
          addUsedNames = true,
          Set(),
          parseMap = false,
          parseTry = false)
        stmts.foreach(exps :+= _.asInstanceOf[Expression])
        parsedExps.push(Index(exp, exps, exp.lineCol))
        nextNode(false)
        nextNode(true)
    }
    parseExpression()
  }

  def parseLambda(): Unit = {
    val lineCol = current.lineCol
    nextNode(false)
    var variableDefList = List[VariableDef]()
    var set = Set[String]()
    current match {
      case e: ElementStartNode =>
        var list = parseElemStart(
          e,
          addUsedNames = false,
          Set(), parseMap = false,
          parseTry = false)
        list.foreach {
          case a: Access =>
            if (a.expression == null) {
              val v = VariableDef(a.name, Set(), null, null, annos, LineCol.SYNTHETIC)
              annos = Set()
              variableDefList :+= v
              set += a.name
            } else
              throw UnexpectedTokenException("variable", a.expression.toString, a.expression.lineCol)
          case v: VariableDef =>
            variableDefList :+= v
            set += v.name
          case x =>
            throw UnexpectedTokenException("variable", x.toString, x.lineCol)
        }
        nextNode(false)
    }
    nextNode(false)
    nextNode(false)
    val parser = Parser(current.asInstanceOf[ElementStartNode])
    set ++= usedVarNames
    parser.addUsedVarNames(set)
    var stmts = parser.parse
    if (stmts.size == 1 && stmts.head.isInstanceOf[Expression]) {
      val ret = Return(stmts.head.asInstanceOf[Expression], stmts.head.lineCol)
      stmts = List()
      stmts :+= ret
    }
    val lambda = Lambda(variableDefList, stmts, lineCol)
    nextNode(true)
    parsedExps.push(lambda)
    parseExpression()
  }

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
    val stmts = parseElemStart(startNode, addUsedNames = true, Set(), parseMap = true, parseTry = false)
    if (stmts.size % 2 != 0)
      throw new SyntaxException("invalid map contents", startNode.lineCol)
    var isKey = true
    var map = Map[Expression, Expression]()
    var exp: Expression = null
    stmts.foreach {
      case e: Expression =>
        if (isKey)
          exp = e
        else
          map += exp -> e
        isKey = !isKey
      case x =>
        throw UnexpectedTokenException("expression", x.toString, x.lineCol)
    }
    MapExp(map, startNode.lineCol)
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
