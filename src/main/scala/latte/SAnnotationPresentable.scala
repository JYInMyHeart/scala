package latte

import scala.collection.mutable.ListBuffer

trait SAnnotationPresentable {
  def annos(): ListBuffer[Anno]
}

trait Value {
  def typeOf(): STypeDef
}

trait LeftValue extends Value {
  def canChange(): Boolean
}

trait Instruction {
  def lineCol(): LineCol
}

class STypeDef(val lineCol: LineCol,
               val annos: ListBuffer[Anno]) extends SAnnotationPresentable {
  var pkg: String = _
  val fullName: String = ""

  def this(lineCol: LineCol) = {
    this (lineCol, ListBuffer())
  }

  def isAssignableFrom(cls: STypeDef): Boolean = {
    cls match {
      case null => throw new NullPointerException()
      case _: NullTypeDef =>
        !this.isInstanceOf[PrimitiveTypeDef]
      case _ => cls == this
    }
  }
}

abstract class PrimitiveTypeDef() extends STypeDef(LineCol.SYNTHETIC)

/**
  * int
  */
case class IntTypeDef() extends PrimitiveTypeDef() {
  override val fullName: String = "int"

  override def isAssignableFrom(cls: STypeDef): Boolean = {
    if (super.isAssignableFrom(cls)) return true
    cls.isInstanceOf[CharTypeDef] || cls.isInstanceOf[ByteTypeDef] || cls.isInstanceOf[ShortTypeDef]
  }
}

object IntTypeDef {
  def t: IntTypeDef = IntTypeDef()

  def get(): IntTypeDef = t
}

/**
  * float
  */
case class FloatTypeDef() extends PrimitiveTypeDef() {
  override val fullName: String = "int"

  override def isAssignableFrom(cls: STypeDef): Boolean = {
    if (super.isAssignableFrom(cls)) return true
    cls.isInstanceOf[IntTypeDef] || IntTypeDef.get().isAssignableFrom(cls)
  }
}

object FloatTypeDef {
  def t: FloatTypeDef = FloatTypeDef()

  def get(): FloatTypeDef = t
}

/**
  * double
  */
case class DoubleTypeDef() extends PrimitiveTypeDef() {
  override val fullName: String = "int"

  override def isAssignableFrom(cls: STypeDef): Boolean = {
    if (super.isAssignableFrom(cls)) return true
    cls.isInstanceOf[FloatTypeDef] || cls.isInstanceOf[LongTypeDef] || FloatTypeDef.get().isAssignableFrom(cls)
  }
}

object DoubleTypeDef {
  def t: DoubleTypeDef = DoubleTypeDef()

  def get(): DoubleTypeDef = t
}

/**
  * long
  */
case class LongTypeDef() extends PrimitiveTypeDef() {
  override val fullName: String = "int"

  override def isAssignableFrom(cls: STypeDef): Boolean = {
    if (super.isAssignableFrom(cls)) return true
    cls.isInstanceOf[IntTypeDef] || IntTypeDef.get().isAssignableFrom(cls)
  }
}

object LongTypeDef {
  def t: LongTypeDef = LongTypeDef()

  def get(): LongTypeDef = t
}

/**
  * char
  */
case class CharTypeDef() extends PrimitiveTypeDef() {
  override val fullName: String = "char"
}

object CharTypeDef {
  def t: CharTypeDef = CharTypeDef()

  def get(): CharTypeDef = t
}

/**
  * short
  */
case class ShortTypeDef() extends PrimitiveTypeDef() {
  override val fullName: String = "short"
}

object ShortTypeDef {
  def t: ShortTypeDef = ShortTypeDef()

  def get(): ShortTypeDef = t
}

/**
  * byte
  */
case class ByteTypeDef() extends PrimitiveTypeDef() {
  override val fullName: String = "byte"
}

object ByteTypeDef {
  def t: ByteTypeDef = ByteTypeDef()

  def get(): ByteTypeDef = t
}

/**
  * boolean
  */
case class BoolTypeDef() extends PrimitiveTypeDef() {
  override val fullName: String = "boolean"
}

object BoolTypeDef {
  def t: BoolTypeDef = BoolTypeDef()

  def get(): BoolTypeDef = t
}

/**
  * null
  */
case class NullTypeDef() extends STypeDef(LineCol.SYNTHETIC) {
  override val fullName: String = "null"
}

object NullTypeDef {
  def t: NullTypeDef = NullTypeDef()

  def get(): NullTypeDef = t
}


abstract class SMember(lineCol: LineCol) extends SAnnotationPresentable {
  val modifiers: ListBuffer[SModifier] = ListBuffer()
  var declaringType: STypeDef = _
  var annos: ListBuffer[Anno] = ListBuffer()
}

abstract class SInvokable(lineCol: LineCol) extends SMember(lineCol) {
  val parameters: ListBuffer[SParameter] = ListBuffer()
  var returnType: STypeDef = _
  val statements: ListBuffer[Instruction] = ListBuffer()
  val exceptionTables: ListBuffer[ExceptionTable] = ListBuffer()
}

case class SParameter() extends LeftValue with SAnnotationPresentable {
  val annos: ListBuffer[Anno] = ListBuffer()
  var name: String = _
  var sType: STypeDef = _
  val canChange: Boolean = true
  var target: SInvokable = _

  override def typeOf(): STypeDef = sType

  override def toString: String =
    s"${if (!canChange) "final" else ""}${sType.fullName} $name"
}

case class SMethodDef(lineCol: LineCol) extends SInvokable(lineCol){
  var name:String = _
  val overRide:ListBuffer[SMethodDef] = ListBuffer()
  val overRidden:ListBuffer[SMethodDef] = ListBuffer()

  override def toString: String = {
    val modifiers = this.modifiers.map(_.toString.toLowerCase()).foldLeft("")(_+" "+_)
    val temp = returnType.fullName + " " + declaringType.fullName + "." + name + "("
    val params = parameters.foldLeft("")(_+","+_)
    modifiers + temp + params + ")"
  }
}

case class ExceptionTable(from: Instruction,
                          to: Instruction,
                          target: Instruction,
                          sType: STypeDef)

class SModifier

object SModifier {
  val PUBLIC = 0x0001
  val PRIVATE = 0x0002
  val PROTECTED = 0x0004
  val STATIC = 0x0008
  val FINAL = 0x0010
  val VOLATILE = 0x0040
  val TRANSIENT = 0x0080
  val ABSTRACT = 0x0400
  val SYNTHETIC = 0x1000
  val ENUM = 0x4000
  val NATIVE = 0x0100
  val STRUCT = 0x0800
  val SYNCHRONIZED = 0x0200
}

