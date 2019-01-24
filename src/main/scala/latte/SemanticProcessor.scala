package latte

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks

class SemanticProcessor(mapOfStatements: mutable.HashMap[String, List[Statement]]) {
  private val types: mutable.HashMap[String, STypeDef] = mutable.HashMap(
    "int" -> IntTypeDef.get(),
    "short" -> ShortTypeDef.get(),
    "byte" -> ByteTypeDef.get(),
    "boolean" -> BoolTypeDef.get(),
    "float" -> FloatTypeDef.get(),
    "long" -> LongTypeDef.get(),
    "char" -> CharTypeDef.get(),
    "double" -> DoubleTypeDef.get(),

  )
  private val originalClasses: mutable.HashMap[String, ClassStatement] = mutable.HashMap()
  private val originalInterfaces: mutable.HashMap[String, InterfaceStatement] = mutable.HashMap()
  private val methodStatements: mutable.HashMap[SMethodDef, ListBuffer[Statement]] = mutable.HashMap()
  private val fileNameToImport: mutable.HashMap[String, ListBuffer[Import]] = mutable.HashMap()
  private val typeDefSet: mutable.HashSet[STypeDef] = mutable.HashSet()

  def parse: mutable.HashSet[STypeDef] = {
    val fileNameToClassDef: mutable.HashMap[String, ListBuffer[ClassStatement]] = mutable.HashMap()
    val fileNameToInterfaceDef: mutable.HashMap[String, ListBuffer[InterfaceStatement]] = mutable.HashMap()
    val fileNameToPackageName: mutable.HashMap[String, String] = mutable.HashMap()
    mapOfStatements.foreach { x =>
      val imports: ListBuffer[Import] = ListBuffer()
      val classDefs: ListBuffer[ClassStatement] = ListBuffer()
      val interfaceDefs: ListBuffer[InterfaceStatement] = ListBuffer()
      val statements = x._2
      var pkg: String = ""
      val loop = new Breaks
      loop.breakable {

        fileNameToImport += x._1 -> imports
        fileNameToClassDef += x._1 -> classDefs
        fileNameToInterfaceDef += x._1 -> interfaceDefs

        val statementIterator = statements.toIterator
        if (statementIterator.hasNext) {
          val stmt = statementIterator.next()
          stmt match {
            case p: PackageDeclare =>
              pkg = p.pkg.pkg.replace("::", ".") + "."
            case _ =>
            //              selectImportClassInterface(stmt, imports, classDefs, interfaceDefs)
          }
          while (statementIterator.hasNext) {
            val stmt = statementIterator.next()
            //            selectImportClassInterface(stmt, imports, classDefs, interfaceDefs)
          }
        } else {
          loop.break()
        }
      }

      imports.+=:(
        Import(
          List(
            ImportDetail(PackageRef(
              if (pkg.endsWith("."))
                pkg.substring(0, pkg.length - 1).replace(".", "::")
              else
                pkg, LineCol.SYNTHETIC
            ),
              null,
              importAll = true)
          ), LineCol.SYNTHETIC
        )
      )

      imports += Import(
        List(ImportDetail(
          PackageRef("java::lang", LineCol.SYNTHETIC), null, true
        )), LineCol.SYNTHETIC
      )
      imports += Import(
        List(ImportDetail(
          PackageRef("lt::lang", LineCol.SYNTHETIC), null, true
        )), LineCol.SYNTHETIC
      )

      fileNameToPackageName += x._1 -> pkg

      val importSimpleNames: mutable.HashSet[String] = mutable.HashSet()
      imports.foreach { i =>
        i.importDetails.foreach { detail =>
          val className = getClassNameFromAccess(detail.access)
          if (!typeExists(className))
            throw new SyntaxException(className + "does not exist", i.lineCol)
          if (importSimpleNames.contains(detail.access.name))
            throw new SyntaxException("duplicate imports", i.lineCol)
          importSimpleNames += detail.access.name
        }
      }
      importSimpleNames.clear()

    }
    null
  }

  def getClassNameFromAccess(access: Access): String = {
    var pre: String = ""
    access.expression match {
      case a: Access =>
        pre = getClassNameFromAccess(a.expression.asInstanceOf[Access]) + "."
      case p: PackageRef =>
        pre = p.pkg.replace("::", ".") + "."
      case _ =>
    }
    pre + access.name
  }

  def typeExists(sType: String): Boolean = {
    if (types.contains(sType)) {
      try {
        Class.forName(sType)
      } catch {
        case _: ClassNotFoundException =>
          return false
      }
    }
    true
  }


}

object SemanticProcessor {
  val PARSING_CLASS = 0
  val PARSING_INTERFACE = 1
}
