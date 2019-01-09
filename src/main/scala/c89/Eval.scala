package c89

import c89.ast._

class Eval(expression: Expression) {
  def eval(): Int = {
    expression match {
      case node: ExpressionTree =>
        new Eval(node.expr).eval()
      case node: NumberNode =>
        node.value.value match {
          case "true" => 0
          case "false" => 1
          case x => x.toInt
        }
      case node: BinaryNode =>
        val left = new Eval(node.left).eval()
        val op = node.op.tokenType
        val right = new Eval(node.right).eval()
        op match {
          case TokenType.add => left + right
          case TokenType.sub => left - right
          case TokenType.plus => left * right
          case TokenType.div => left / right
          case TokenType.pow => math.pow(left, right).toInt
          case TokenType.mod => left % right
          case TokenType.or => if (left == 0 || right == 0) 0 else 1
          case TokenType.and => if (left == 0 && right == 0) 0 else 1
        }
      case node: BraceNode =>
        new Eval(node.op).eval()
      case node: UnaryNode =>
        val value = new Eval(node.oprand).eval()
        node.op.asInstanceOf[Tokens].tokenType match {
          case TokenType.add =>
            value
          case TokenType.sub =>
            -value
          case TokenType.not =>
            if (value == 0)
              1
            else
              0
        }

      case _ => throw new LexerException("unknown node type")
    }


  }

}
