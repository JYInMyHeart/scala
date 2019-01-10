package c89

class Eval(expression: BindExpression) {
  def eval(): Any = {
    expression match {
      case node: BindLiteralExpression =>
        node.value match {
          case i: Int => i
          case i: Boolean => i
          case _ =>
            throw new Exception(s"unknown literal type")
        }
      case node: BindBinaryExpression =>
        val left = new Eval(node.boundLeft).eval()
        val right = new Eval(node.boundRight).eval()
        val op = node.bindType
        (left, right, op) match {
          case (l: Int, r: Int, BindType.addition) => l + r
          case (l: Int, r: Int, BindType.subtraction) => l - r
          case (l: Int, r: Int, BindType.multiplication) => l * r
          case (l: Int, r: Int, BindType.division) => l / r
          case (l: Int, r: Int, BindType.pow) => math.pow(l, r)
          case (l: Int, r: Int, BindType.mod) => l % r
          case (l: Int, r: Int, BindType.lt ) => l < r
          case (l: Int, r: Int, BindType.lte) => l <= r
          case (l: Int, r: Int, BindType.gt ) => l > r
          case (l: Int, r: Int, BindType.gte) => l >= r
          case (l: Int, r: Int, BindType.equal) => l == r
          case (l: Boolean, r: Boolean, BindType.and) => l && r
          case (l: Boolean, r: Boolean, BindType.or) => l || r
          case (l: Boolean, r: Boolean, BindType.equal) => l == r
          case _ =>
            throw new Exception(s"unknown literal type")
        }

      case node: BindUnaryExpression =>
        val value = new Eval(node.boundOperand).eval()
        (value, node.bindType) match {
          case (o: Boolean, BindType.not) => !o
          case (o: Int, BindType.negation) => -o
          case (o: Int, BindType.identity) => o
        }
      case _ => throw new LexerException("unknown node type")
    }
  }
}
