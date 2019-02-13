package latte

import scala.collection.{mutable => mu}

class SemanticScope {
  case class MethodRecorder(method:SMethodDef,
                            paramCount:Int) {

  }

  var parent:SemanticScope = _
  val leftValueMap:mu.HashMap[String,LeftValue] = mu.HashMap()
  val innerMethodMap:mu.HashMap[String,MethodRecorder] = mu.HashMap()

  var STypeDef:STypeDef = _


}
