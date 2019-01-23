package latte

import java.io.{BufferedReader, StringReader}

import latte.ParserTest.parse

class ParserTest extends UnitSpec {

  "testOperatorInSamePriority" should "1+2-3+4" in {
    val statements = parse("1+2-3+4")
    assert(1 == statements.size)
    val statement = statements.head
    val one = NumberLiteral("1", LineCol.SYNTHETIC)
    val two = NumberLiteral("2", LineCol.SYNTHETIC)
    val tvo1 = TwoVariableOperation("+", one, two, LineCol.SYNTHETIC)
    val three = NumberLiteral("3", LineCol.SYNTHETIC)
    val tvo2 = TwoVariableOperation("-", tvo1, three, LineCol.SYNTHETIC)
    val four = NumberLiteral("4", LineCol.SYNTHETIC)
    val tvo3 = TwoVariableOperation("+", tvo2, four, LineCol.SYNTHETIC)
    assert(tvo3 == statement)
  }

  "testOperatorInDifferentPriorities" should "1+2*3+4" in {
    val statements = parse("1+2*3+4")
    assert(1 == statements.size)
    val statement = statements.head
    val one = NumberLiteral("1", LineCol.SYNTHETIC)
    val two = NumberLiteral("2", LineCol.SYNTHETIC)
    val three = NumberLiteral("3", LineCol.SYNTHETIC)
    val four = NumberLiteral("4", LineCol.SYNTHETIC)
    val tvo1 = TwoVariableOperation("*", two, three, LineCol.SYNTHETIC)
    val tvo2 = TwoVariableOperation("+", one, tvo1, LineCol.SYNTHETIC)
    val tvo3 = TwoVariableOperation("+", tvo2, four, LineCol.SYNTHETIC)
    assert(tvo3 == statement)
  }

  "test 1 plus 2" should "nice" in {
    val statements = parse("1*2")
    assert(1 == statements.size)
    val statement = statements.head
    val one = NumberLiteral("1", LineCol.SYNTHETIC)
    val two = NumberLiteral("2", LineCol.SYNTHETIC)
    val tvo1 = TwoVariableOperation("*", one, two, LineCol.SYNTHETIC)
    assert(tvo1 == statement)
  }

  "test1Plus2Multi3" should "nice" in {
    val statements = parse("1+2*3")
    assert(1 == statements.size)
    val statement = statements.head
    val one = NumberLiteral("1", LineCol.SYNTHETIC)
    val two = NumberLiteral("2", LineCol.SYNTHETIC)
    val three = NumberLiteral("3", LineCol.SYNTHETIC)
    val tvo1 = TwoVariableOperation("*", two, three, LineCol.SYNTHETIC)
    val tvo2 = TwoVariableOperation("+", one, tvo1, LineCol.SYNTHETIC)
    assert(tvo2 == statement)
  }


  "test1Plus2Multi3Div4" should "nice" in {
    val statements = parse("1+2*3/4")
    assert(1 == statements.size)
    val statement = statements.head
    val one = NumberLiteral("1", LineCol.SYNTHETIC)
    val two = NumberLiteral("2", LineCol.SYNTHETIC)
    val three = NumberLiteral("3", LineCol.SYNTHETIC)
    val four = NumberLiteral("4", LineCol.SYNTHETIC)
    val tvo1 = TwoVariableOperation("*", two, three, LineCol.SYNTHETIC)
    val tvo2 = TwoVariableOperation("/", tvo1, four, LineCol.SYNTHETIC)
    val tvo3 = TwoVariableOperation("+", one, tvo2, LineCol.SYNTHETIC)
    assert(tvo3 == statement)
  }


  "test1Plus2Multi3Div4Minus5" should "nice" in {
    val statements = parse("1+2*3/4-5")
    assert(1 == statements.size)
    val statement = statements.head
    val one = NumberLiteral("1", LineCol.SYNTHETIC)
    val two = NumberLiteral("2", LineCol.SYNTHETIC)
    val three = NumberLiteral("3", LineCol.SYNTHETIC)
    val four = NumberLiteral("4", LineCol.SYNTHETIC)
    val five = NumberLiteral("5", LineCol.SYNTHETIC)
    val tvo1 = TwoVariableOperation("*", two, three, LineCol.SYNTHETIC)
    val tvo2 = TwoVariableOperation("/", tvo1, four, LineCol.SYNTHETIC)
    val tvo3 = TwoVariableOperation("+", one, tvo2, LineCol.SYNTHETIC)
    val tvo4 = TwoVariableOperation("-", tvo3, five, LineCol.SYNTHETIC)
    assert(tvo4 == statement)
  }


  "testPar1Plus2ParMulti3" should "nice" in {
    val statements = parse("(1+2)*3")
    assert(1 == statements.size)
    val statement = statements.head
    val one = NumberLiteral("1", LineCol.SYNTHETIC)
    val two = NumberLiteral("2", LineCol.SYNTHETIC)
    val three = NumberLiteral("3", LineCol.SYNTHETIC)

    val tvo1 = TwoVariableOperation("+", one, two, LineCol.SYNTHETIC)
    val tvo2 = TwoVariableOperation("*", tvo1, three, LineCol.SYNTHETIC)

    assert(tvo2 == statement)
  }


  "testBinOperator" should "nice" in {
    val statements = parse("1*3/(4+5)*6-(7/8+9)-10-15")
    assert(1 == statements.size)
    val statement = statements.head
    val one = NumberLiteral("1", LineCol.SYNTHETIC)
    val three = NumberLiteral("3", LineCol.SYNTHETIC)
    val four = NumberLiteral("4", LineCol.SYNTHETIC)
    val five = NumberLiteral("5", LineCol.SYNTHETIC)
    val six = NumberLiteral("6", LineCol.SYNTHETIC)
    val seven = NumberLiteral("7", LineCol.SYNTHETIC)
    val eight = NumberLiteral("8", LineCol.SYNTHETIC)
    val nine = NumberLiteral("9", LineCol.SYNTHETIC)
    val ten = NumberLiteral("10", LineCol.SYNTHETIC)
    val fifteen = NumberLiteral("15", LineCol.SYNTHETIC)

    val oneMulThree = TwoVariableOperation("*", one, three, LineCol.SYNTHETIC)
    val fourPlusFive = TwoVariableOperation("+", four, five, LineCol.SYNTHETIC)
    val DIV1 = TwoVariableOperation("/", oneMulThree, fourPlusFive, LineCol.SYNTHETIC)
    val MUL1 = TwoVariableOperation("*", DIV1, six, LineCol.SYNTHETIC)
    val sevenDIVIDEeight = TwoVariableOperation("/", seven, eight, LineCol.SYNTHETIC)
    val DIVPLUSnine = TwoVariableOperation("+", sevenDIVIDEeight, nine, LineCol.SYNTHETIC)
    val MINUS1 = TwoVariableOperation("-", MUL1, DIVPLUSnine, LineCol.SYNTHETIC)
    val MINUS10 = TwoVariableOperation("-", MINUS1, ten, LineCol.SYNTHETIC)
    val MINUS15 = TwoVariableOperation("-", MINUS10, fifteen, LineCol.SYNTHETIC)
    assert(MINUS15 == statement)
  }

  "testOperators" should "nice" in {
    val statements = parse("+1++ -3^!true+2+\"abc\"")
    assert(1 == statements.size)
    val statement = statements.head
    val one = NumberLiteral("1", LineCol.SYNTHETIC)
    val postPlusPlus = OneVariableOperation("++", one, LineCol.SYNTHETIC)
    val plusOne = UnaryOneVariableOperation("+", postPlusPlus, LineCol.SYNTHETIC)
    val three = NumberLiteral("3", LineCol.SYNTHETIC)
    val minus = TwoVariableOperation("-", plusOne, three, LineCol.SYNTHETIC)

    val tr = BoolLiteral("true", LineCol.SYNTHETIC)
    val not = UnaryOneVariableOperation("!", tr, LineCol.SYNTHETIC)

    val two = NumberLiteral("2", LineCol.SYNTHETIC)
    val plusTwo = TwoVariableOperation("+", not, two, LineCol.SYNTHETIC)
    val abc = StringLiteral("\"abc\"", LineCol.SYNTHETIC)
    val plusABC = TwoVariableOperation("+", plusTwo, abc, LineCol.SYNTHETIC)

    val xor = TwoVariableOperation("^", minus, plusABC, LineCol.SYNTHETIC)
    assert(xor == statement)
  }

  "testPost1VarWithOperatorPriority" should "nice" in {
    val statements = parse("1+1++ *1")
    assert(1 == statements.size)
    val statement = statements.head

    val n1 = NumberLiteral("1", LineCol.SYNTHETIC)
    val n2 = NumberLiteral("1", LineCol.SYNTHETIC)
    val n3 = NumberLiteral("1", LineCol.SYNTHETIC)

    val ovo = OneVariableOperation("++", n2, LineCol.SYNTHETIC)
    val tvo1 = TwoVariableOperation("*", ovo, n3, LineCol.SYNTHETIC)
    val tvo2 = TwoVariableOperation("+", n1, tvo1, LineCol.SYNTHETIC)
    assert(tvo2 == statement)
  }


  "testPackage" should "nice" in {
    val statements = parse("java::lang::Integer")
    assert(1 == statements.size)
    val statement = statements.head
    val pkg = PackageRef("java::lang", LineCol.SYNTHETIC)
    val a = Access(pkg, "Integer", LineCol.SYNTHETIC)
    assert(a == statement)
  }

  "testPkgAccess" should "nice" in {
    val statements = parse("java::lang::String.cls")
    assert(1 == statements.size)
    val statement = statements.head
    val pkg = PackageRef("java::lang", LineCol.SYNTHETIC)
    val access = Access(pkg, "String", LineCol.SYNTHETIC)
    val access2 = Access(access, "cls", LineCol.SYNTHETIC)
    assert(access2 == statement)
  }

  "testInvocation" should "nice" in {
    val statements = parse("java::lang::String.valueOf(true)")
    assert(1 == statements.size)
    val statement = statements.head
    val pkg = PackageRef("java::lang", LineCol.SYNTHETIC)
    val access = Access(pkg, "String", LineCol.SYNTHETIC)
    val access2 = Access(access, "valueOf", LineCol.SYNTHETIC)

    val invocation = Invocation(access2, List[Expression](BoolLiteral("true", LineCol.SYNTHETIC)), LineCol.SYNTHETIC)
    assert(statement == invocation)
  }

  "testInvocationNoArg" should "nice" in {
    val statements = parse("method()")
    assert(1 == statements.size)
    val statement = statements.head
    val invocation = Invocation(Access(null, "method", LineCol.SYNTHETIC), List(), LineCol.SYNTHETIC)
    assert(invocation == statement)
  }

  "testVariableWithInitValue" should "nice" in {
    val statements = parse("i=1")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val n = NumberLiteral("1", LineCol.SYNTHETIC)
    v.init = n
    assert(statement == v)
  }

  "testVariableWithInitType_FullName" should "nice" in {
    val statements = parse("i:java::lang::Integer")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val pkg = PackageRef("java::lang", LineCol.SYNTHETIC)
    val access = Access(pkg, "Integer", LineCol.SYNTHETIC)
    v.vType = access
    assert(statement == v)
  }

  "testVariableWithInitType_SimpleName" should "nice" in {
    val statements = parse("i:Integer")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val access = Access(null, "Integer", LineCol.SYNTHETIC)
    v.vType = access
    assert(statement == v)
  }

  "testVariableWithInitType_FullName_Inner" should "nice" in {
    val statements = parse("i:mePackage::ClassName.Inner")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val pkg = PackageRef("mePackage", LineCol.SYNTHETIC)
    val access1 = Access(pkg, "ClassName", LineCol.SYNTHETIC)
    val access2 = Access(access1, "Inner", LineCol.SYNTHETIC)
    v.vType = access2
    assert(statement == v)
  }

  "testVariableWithInitType_SimpleName_Inner" should "nice" in {
    val statements = parse("i:ClassName.Inner")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val access1 = Access(null, "ClassName", LineCol.SYNTHETIC)
    val access2 = Access(access1, "Inner", LineCol.SYNTHETIC)
    v.vType = access2
    assert(statement == v)
  }


  "testVariableWithInitType_FullName_Init" should "nice" in {
    val statements = parse("i:java::lang::Integer=1")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val pkg = PackageRef("java::lang", LineCol.SYNTHETIC)
    val access = Access(pkg, "Integer", LineCol.SYNTHETIC)
    v.vType = access
    val n = NumberLiteral("1", LineCol.SYNTHETIC)
    v.init = n
    assert(statement == v)
  }


  "testVariableWithInitType_SimpleName_Init" should "nice" in {
    val statements = parse("i:Integer=1")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val access = Access(null, "Integer", LineCol.SYNTHETIC)
    v.vType = access
    val n = NumberLiteral("1", LineCol.SYNTHETIC)
    v.init = n
    assert(statement == v)
  }


  "testVariableWithInitType_FullName_Inner_Init" should "nice" in {
    val statements = parse("i:mePackage::ClassName.Inner=1")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val pkg = PackageRef("mePackage", LineCol.SYNTHETIC)
    val access1 = Access(pkg, "ClassName", LineCol.SYNTHETIC)
    val access2 = Access(access1, "Inner", LineCol.SYNTHETIC)
    v.vType = access2
    val n = NumberLiteral("1", LineCol.SYNTHETIC)
    v.init = n
    assert(statement == v)
  }

  "testVariableWithInitType_SimpleName_Inner_Init" should "nice" in {
    val statements = parse("i:ClassName.Inner=1")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val access1 = Access(null, "ClassName", LineCol.SYNTHETIC)
    val access2 = Access(access1, "Inner", LineCol.SYNTHETIC)
    v.vType = access2
    val n = NumberLiteral("1", LineCol.SYNTHETIC)
    v.init = n
    assert(statement == v)
  }

  "testVariableWithInitType_SimpleName_Inner_Init_Operator" should "nice" in {
    val statements = parse("i:ClassName.Inner=1+2")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val access1 = Access(null, "ClassName", LineCol.SYNTHETIC)
    val access2 = Access(access1, "Inner", LineCol.SYNTHETIC)
    v.vType = access2
    val n = NumberLiteral("1", LineCol.SYNTHETIC)
    val n2 = NumberLiteral("2", LineCol.SYNTHETIC)
    val o = TwoVariableOperation("+", n, n2, LineCol.SYNTHETIC)
    v.init = o
    assert(statement == v)
  }


  "testModifier" should "nice" in {
    val statements = parse("val i:ClassName.Inner=1+2")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(Modifier("val", LineCol.SYNTHETIC)), null, null, Set(), LineCol.SYNTHETIC)
    val access1 = Access(null, "ClassName", LineCol.SYNTHETIC)
    val access2 = Access(access1, "Inner", LineCol.SYNTHETIC)
    v.vType = access2
    val n = NumberLiteral("1", LineCol.SYNTHETIC)
    val n2 = NumberLiteral("2", LineCol.SYNTHETIC)
    val o = TwoVariableOperation("+", n, n2, LineCol.SYNTHETIC)
    v.init = o
    assert(statement == v)
  }


  "testAssign" should "nice" in {
    val statements = parse("i=1\ni=2")
    assert(2 == statements.size)
    val statement = statements(1)
    val access = Access(null, "i", LineCol.SYNTHETIC)
    val n = NumberLiteral("2", LineCol.SYNTHETIC)
    val ass = Assignment(access, "=", n, LineCol.SYNTHETIC)
    assert(ass == statement)
  }


  "testMethodNormal_Noparam" should "nice" in {
    val statements = parse(
      "" + "method()\n"
        + "    a=false"
    )
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("a", Set(), null, null, Set(), LineCol.SYNTHETIC)
    v.init = BoolLiteral("false", LineCol.SYNTHETIC)
    val methodStatement = MethodStatement(
      "method",
      Set(),
      null,
      List(),
      Set(),
      List(v),
      LineCol.SYNTHETIC
    )
    assert(methodStatement == statement)
  }

  "testMethodType_NoParam" should "nice" in {
    val statements = parse(
      "" + "method():Integer\n"
        + "    a=false"
    )
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("a", Set(), null, null, Set(), LineCol.SYNTHETIC)
    v.init = BoolLiteral("false", LineCol.SYNTHETIC)
    val methodStatement = MethodStatement(
      "method",
      Set(),
      Access(null, "Integer", LineCol.SYNTHETIC),
      List(),
      Set(),
      List(v),
      LineCol.SYNTHETIC
    )
    assert(methodStatement == statement)
  }


  "testMethodType_NoParam_NoStmt" should "nice" in {
    val statements = parse(
      "" + "method():Integer"
    )
    assert(1 == statements.size)
    val statement = statements.head
    val methodStatement = MethodStatement(
      "method",
      Set(),
      Access(null, "Integer", LineCol.SYNTHETIC),
      List(),
      Set(),
      List(),
      LineCol.SYNTHETIC
    )
    assert(methodStatement == statement)
  }

  "testMethodEmpty_NoParam" should "nice" in {
    val statements = parse(
      "" + "method()=..."
    )
    assert(1 == statements.size)
    val statement = statements.head
    val methodStatement = MethodStatement(
      "method",
      Set(),
      null,
      List(),
      Set(),
      List(),
      LineCol.SYNTHETIC
    )
    assert(methodStatement == statement)
  }

  "testMethodEmpty_Stmt_NoParam" should "nice" in {
    val statements = parse(
      "" + "method()=123"
    )
    assert(1 == statements.size)
    val statement = statements.head
    val methodStatement = MethodStatement(
      "method",
      Set(),
      null,
      List(),
      Set(),
      List(
        Return(
          NumberLiteral("123", LineCol.SYNTHETIC),
          LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    assert(methodStatement == statement)
  }


  "testMethodGeneral" should "nice" in {
    val statements = parse(
      "" + "abs method(a,b:Character):Integer\n" + "    a=false"
    )
    assert(1 == statements.size)
    val statement = statements.head
    val a = VariableDef("a", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val b = VariableDef("b", Set(), null, null, Set(), LineCol.SYNTHETIC)
    b.vType = Access(null, "Character", LineCol.SYNTHETIC)
    val vars = List(a, b)
    val assignment = Assignment(
      Access(null, "a", LineCol.SYNTHETIC),
      "=",
      BoolLiteral("false", LineCol.SYNTHETIC),
      LineCol.SYNTHETIC)
    val methodStatement = MethodStatement(
      "method",
      Set(Modifier("abs", LineCol.SYNTHETIC)),
      Access(null, "Integer", LineCol.SYNTHETIC),
      vars,
      Set(),
      List(assignment),
      LineCol.SYNTHETIC
    )
    assert(methodStatement == statement)
  }


  "testReturn" should "nice" in {
    val statements = parse(
      "<i+1"
    )
    assert(1 == statements.size)
    val statement = statements.head
    val access = Access(null, "i", LineCol.SYNTHETIC)
    val one = NumberLiteral("1", LineCol.SYNTHETIC)
    val tvo = TwoVariableOperation("+", access, one, LineCol.SYNTHETIC)
    val ret = Return(tvo, LineCol.SYNTHETIC)
    assert(statement == ret)
  }

  "testReturnVoid" should "nice" in {
    val statements = parse(
      "<"
    )
    assert(1 == statements.size)
    val statement = statements.head
    assert(statement.asInstanceOf[Return].exp == null)
  }

  "testJavaName" should "nice" in {
    try {
      parse("sync=1")
    }
    catch {
      case _: Exception =>
    }
    val statements = parse(
      "`sync`=1"
    )
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("sync", Set(), null, null, Set(), LineCol.SYNTHETIC)
    v.init = NumberLiteral("1", LineCol.SYNTHETIC)
    assert(v == statement)
  }

  "testIf" should "nice" in {
    val statements = parse(
      "if true"
    )
    assert(1 == statements.size)
    val statement = statements.head
    val ifStatement = IfStatement(
      List(
        IfPair(BoolLiteral("true", LineCol.SYNTHETIC),
          List(),
          LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    assert(ifStatement == statement)
  }


  "testIfBody" should "nice" in {
    val statements = parse(
      "if true\n" +
        "    a=1"
    )
    assert(1 == statements.size)
    val statement = statements.head
    val ifStatement = IfStatement(
      List(
        IfPair(BoolLiteral("true", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("1", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    assert(ifStatement == statement)
  }

  "testIfBodyElseIf" should "nice" in {
    val statements = parse(
      "if true\n" +
        "    a=1\n" +
        "elseif boolval"
    )
    assert(1 == statements.size)
    val statement = statements.head

    val ifStatement = IfStatement(
      List(
        IfPair(
          BoolLiteral("true", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("1", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC),
        IfPair(
          Access(null, "boolval", LineCol.SYNTHETIC),
          List(),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(ifStatement == statement)
  }


  "testIfBodyElseIfBody" should "nice" in {
    val statements = parse(
      "if true\n" +
        "    a=1\n" +
        "elseif boolval\n" +
        "    a=2"
    )
    assert(1 == statements.size)
    val statement = statements.head

    val ifStatement = IfStatement(
      List(
        IfPair(
          BoolLiteral("true", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("1", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC),
        IfPair(
          Access(null, "boolval", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("2", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(ifStatement == statement)
  }


  "testIfBodyElseIfBodyElse" should "nice" in {
    val statements = parse(
      "if true\n" +
        "    a=1\n" +
        "elseif boolval\n" +
        "    a=2\n" +
        "else\n"
    )
    assert(1 == statements.size)
    val statement = statements.head

    val ifStatement = IfStatement(
      List(
        IfPair(
          BoolLiteral("true", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("1", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC),
        IfPair(
          Access(null, "boolval", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("2", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC
        ),
        IfPair(
          null,
          List(),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(ifStatement == statement)
  }


  "testIfBodyElseIfBodyElseBody" should "nice" in {
    val statements = parse(
      "if true\n" +
        "    a=1\n" +
        "elseif boolval\n" +
        "    a=2\n" +
        "else\n" +
        "    a=3"
    )
    assert(1 == statements.size)
    val statement = statements.head

    val ifStatement = IfStatement(
      List(
        IfPair(
          BoolLiteral("true", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("1", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC),
        IfPair(
          Access(null, "boolval", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("2", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC
        ),
        IfPair(
          null,
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("3", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(ifStatement == statement)
  }


  "testIfBodyElseIfBodyElseBody_1" should "nice" in {
    val statements = parse(
      "if true\n" +
        "    a=1\n" +
        "elseif boolval1\n" +
        "elseif boolval2\n" +
        "else\n" +
        "    a=3"
    )
    assert(1 == statements.size)
    val statement = statements.head

    val ifStatement = IfStatement(
      List(
        IfPair(
          BoolLiteral("true", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("1", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC),
        IfPair(
          Access(null, "boolval1", LineCol.SYNTHETIC),
          List(),
          LineCol.SYNTHETIC
        ),
        IfPair(
          Access(null, "boolval2", LineCol.SYNTHETIC),
          List(),
          LineCol.SYNTHETIC
        ),
        IfPair(
          null,
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("3", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(ifStatement == statement)
  }

  "testIfBodyElseIfBodyElseBody_2" should "nice" in {
    val statements = parse(
      "if true\n" +
        "    a=1\n" +
        "elseif boolval1\n" +
        "    a=2\n" +
        "elseif boolval2\n" +
        "else\n" +
        "    a=3"
    )
    assert(1 == statements.size)
    val statement = statements.head

    val ifStatement = IfStatement(
      List(
        IfPair(
          BoolLiteral("true", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("1", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC),
        IfPair(
          Access(null, "boolval1", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("2", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC
        ),
        IfPair(
          Access(null, "boolval2", LineCol.SYNTHETIC),
          List(),
          LineCol.SYNTHETIC
        ),
        IfPair(
          null,
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("3", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(ifStatement == statement)
  }


  "testIfBodyElseIfElseBody" should "nice" in {
    val statements = parse(
      "if true\n" +
        "    a=1\n" +
        "elseif boolval2\n" +
        "else\n" +
        "    a=3"
    )
    assert(1 == statements.size)
    val statement = statements.head

    val ifStatement = IfStatement(
      List(
        IfPair(
          BoolLiteral("true", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("1", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC),
        IfPair(
          Access(null, "boolval1", LineCol.SYNTHETIC),
          List(),
          LineCol.SYNTHETIC
        ),
        IfPair(
          null,
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("3", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(ifStatement == statement)
  }

  "testFor1" should "nice" in {
    val statements = parse("for i in ite")
    assert(statements.size == 1)
    val s = statements.head
    val forStatement = ForStatement(
      "i",
      Access(null, "ite", LineCol.SYNTHETIC),
      List(),
      LineCol.SYNTHETIC
    )
    assert(s == forStatement)
  }

  "testFor2" should "nice" in {
    val statements = parse("for i in ite\n    a=1")
    assert(statements.size == 1)
    val s = statements.head
    val forStatement = ForStatement(
      "i",
      Access(null, "ite", LineCol.SYNTHETIC),
      List(
        VariableDef("a",
          Set(),
          null,
          NumberLiteral("1", LineCol.SYNTHETIC),
          Set(),
          LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    assert(s == forStatement)
  }

  "testLambdaNoParam" should "nice" in {
    val statements = parse("()->1 + 1")
    assert(statements.size == 1)
    val s = statements.head
    val lambda = Lambda(
      List(),
      List(
        Return(
          TwoVariableOperation(
            "+",
            NumberLiteral("1", LineCol.SYNTHETIC),
            NumberLiteral("1", LineCol.SYNTHETIC),
            LineCol.SYNTHETIC
          ),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(s == lambda)
  }

  "testLambda2Param" should "nice" in {
    val statements = parse("(a,b)->a+b")
    assert(statements.size == 1)
    val s = statements.head
    val lambda = Lambda(
      List(
        VariableDef("a",
          Set(),
          null,
          null,
          Set(),
          LineCol.SYNTHETIC),
        VariableDef("b",
          Set(),
          null,
          null,
          Set(),
          LineCol.SYNTHETIC)
      ),
      List(
        Return(
          TwoVariableOperation(
            "+",
            Access(null, "a", LineCol.SYNTHETIC),
            Access(null, "b", LineCol.SYNTHETIC),
            LineCol.SYNTHETIC
          ),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(s == lambda)
  }

  "testLambdaInMethodInvocation" should "nice" in {
    val statements = parse("" + "method(\n" + "    (a)->a+1\n" + "    1" + ")")
    assert(statements.size == 1)
    val s = statements.head
    val invocation = Invocation(
      Access(null, "method", LineCol.SYNTHETIC),
      List(
        Lambda(
          List(
            VariableDef("a",
              Set(),
              null,
              null,
              Set(),
              LineCol.SYNTHETIC),
          ),
          List(
            Return(
              TwoVariableOperation(
                "+",
                Access(null, "a", LineCol.SYNTHETIC),
                NumberLiteral("1", LineCol.SYNTHETIC),
                LineCol.SYNTHETIC
              ),
              LineCol.SYNTHETIC
            )
          ),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )

    assert(s == invocation)
  }

  "testStatic1" should "nice" in {
    val statements = parse("static a=1")
    assert(statements.size == 1)
    val s = statements.head
    val ss = StaticScope(
      List(
        VariableDef("a",
          Set(),
          null,
          NumberLiteral("1", LineCol.SYNTHETIC),
          Set(),
          LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    assert(ss == s)
  }

  "testStatic2" should "nice" in {
    val statements = parse("static\n    a=1\n    b=2")
    assert(statements.size == 1)
    val s = statements.head
    val ss = StaticScope(
      List(
        VariableDef("a",
          Set(),
          null,
          NumberLiteral("1", LineCol.SYNTHETIC),
          Set(),
          LineCol.SYNTHETIC),
        VariableDef("b",
          Set(),
          null,
          NumberLiteral("2", LineCol.SYNTHETIC),
          Set(),
          LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    assert(ss == s)
  }






}

object ParserTest {
  def parse(stmt: String): List[Statement] = {
    val processor = new Lexer("test", new BufferedReader(new StringReader(stmt)), 4)
    val root = processor.parse
    val syntacticProcessor = Parser(root)
    syntacticProcessor.parse
  }
}
