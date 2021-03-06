package c89

import c89.TokenType.TokenType

import scala.collection.mutable.ListBuffer

class Diagnostics(val span: Span,
                  val message: String) {
  override def toString: String = message
}

object Diagnostics {
  def apply(span: Span,
            message: String
           ): Diagnostics = new Diagnostics(
    span,
    message
  )
}

class DiagnosticsBag {


  val reports: ListBuffer[Diagnostics] = new ListBuffer[Diagnostics]()

  def report(span: Span, msg: String): reports.type = {
    reports += Diagnostics(span, msg)
  }

  def reportInvalidNumber(span: Span, text: String, clazz: String): Unit = {
    val msg = s"The number $text isn't valid $clazz at $span."
    report(span, msg)
  }

  def reportBadCharacter(position: Int, char: Char): Unit = {
    val span = Span(position, 0)
    val msg = s"Bad char input: '$char' at $span."
    report(span, msg)
  }

  def reportUnexpectedToken(span: Span, actualType: TokenType, expectedType: TokenType): Unit = {
    val msg = s"Unexpected token <$actualType>, expected <$expectedType> at $span."
    report(span, msg)
  }

  def reportUndefinedUnaryOperator(span: Span, operatorText: String, operandType: String): Unit = {
    val msg = s"Unary operator '$operatorText' is not defined for types $operandType at $span."
    report(span, msg)
  }

  def reportUndefinedBinaryOperator(span: Span, operatorText: String, leftType: String, rightType: String): Unit = {
    val msg = s"Binary operator '$operatorText' is not defined for types $leftType and $rightType at $span."
    report(span, msg)
  }

  def reportUndefinedName(span: Span, name: String) = {
    val msg = s"Undefined variable $name at $span"
    report(span,msg)
  }

  def concat(diagnosticsBag: DiagnosticsBag):Unit = {
    reports ++= diagnosticsBag.reports
  }

  def isEmpty: Boolean = reports.isEmpty
}
object DiagnosticsBag{
  def apply(): DiagnosticsBag = new DiagnosticsBag()
}


