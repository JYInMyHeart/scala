package c89

import c89.ast.SyntaxTree

class Compilation(ast: SyntaxTree) {
  def evaluate(): EvaluationResult = {
    val binder = Binder()
    val boundExpression = binder.bindExpression(ast.root)
    ast.diagnostics.concat(binder.diagnostics)
    val diagnostics = ast.diagnostics
    if (!diagnostics.isEmpty)
      return EvaluationResult(diagnostics, null.asInstanceOf[AnyVal])
    val evaluator = Eval(boundExpression)
    val value = evaluator.eval(binder.variables)
    EvaluationResult(DiagnosticsBag(), value)
  }
}
