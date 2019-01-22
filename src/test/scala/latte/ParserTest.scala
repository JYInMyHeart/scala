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


}

object ParserTest {
  def parse(stmt: String): List[Statement] = {
    val processor = new Lexer("test", new BufferedReader(new StringReader(stmt)), 4)
    val root = processor.parse
    val syntacticProcessor = Parser(root)
    syntacticProcessor.parse
  }
}
