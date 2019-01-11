package c89

import c89.ast.SyntaxTree

import scala.collection.mutable

class Compilation(ast: SyntaxTree) {
  def evaluate(variables: mutable.HashMap[VariableSymbol,AnyVal]): EvaluationResult = {
    val binder = Binder()
    val boundExpression = binder.bindExpression(ast.root,variables)
    ast.diagnostics.concat(binder.diagnostics)
    val diagnostics = ast.diagnostics
    if (!diagnostics.isEmpty)
      return EvaluationResult(diagnostics, null.asInstanceOf[AnyVal])
    val evaluator = Eval(boundExpression)
    variables ++= binder.variables
    val value = evaluator.eval(variables)
    EvaluationResult(DiagnosticsBag(), value)
  }
}
