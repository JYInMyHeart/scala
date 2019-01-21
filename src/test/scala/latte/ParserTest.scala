package latte

import java.io.{BufferedReader, StringReader}

import latte.ParserTest.parse

class ParserTest extends UnitSpec {

  "operatorInSamePriority" should "nice" in {
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

}

object ParserTest {
  def parse(stmt: String): List[Statement] = {
    val processor = new Lexer("test", new BufferedReader(new StringReader(stmt)), 4)
    val root = processor.parse
    val syntacticProcessor = Parser(root)
    syntacticProcessor.parse
  }
}
