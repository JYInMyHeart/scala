package c89

import c89.ast._
class Eval(expression: Expression) {
  def eval():Int = {
    expression match {
      case node:ExpressionTree =>
        new Eval(node.expr).eval()
      case node: NumberNode =>
         node.value.value.toInt
      case node:BinaryNode =>
        val left = new Eval(node.left).eval()
        val op = node.op.tokenType
        val right = new Eval(node.right).eval()
        op match {
          case TokenType.add => left + right
          case TokenType.sub => left - right
          case TokenType.plus => left * right
          case TokenType.div => left / right
        }
      case node:BraceNode =>
        new Eval(node.op).eval()
      case _ => throw new LexerException("unknown node type")
    }



  }

}
