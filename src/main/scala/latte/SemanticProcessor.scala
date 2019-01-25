package latte

import java.lang.reflect.{AnnotatedElement, Constructor}

import latte.SModifier.{ABSTRACT, FINAL, PUBLIC}

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

      classDefs.foreach { c =>
        val className = pkg + c.name
        if (types.contains(className))
          throw new SyntaxException(s"duplicate type names $className", c.lineCol)

        val sClassDef = SClassDef(c.lineCol)
        sClassDef.fullName = className
        sClassDef.pkg = if (pkg.endsWith(".")) pkg.substring(0, pkg.length - 1) else pkg
        sClassDef.modifiers += PUBLIC

        def getModifier(m: Modifier): Option[SModifier] =
          m.modifier match {
            case "abs" => Some(ABSTRACT)
            case "val" => Some(FINAL)
            case "pub" => None
            case "pri" => None
            case "pro" => None
            case "pkg" => None
            case _ =>
              throw UnexpectedTokenException("valid modifier for class (val|abs)", m.toString, m.lineCol)
          }


        c.modifiers.map(getModifier).foreach {
          case Some(xx) => sClassDef.modifiers += xx
          case None =>
        }

        types += className -> sClassDef
        originalClasses += className -> c
        typeDefSet += sClassDef
      }

      interfaceDefs.foreach { i =>
        val interfaceName = pkg + i.name
        if (types.contains(interfaceName))
          throw new SyntaxException(s"duplicate type names $interfaceName", i.lineCol)

        val sInterfaceDef = SInterfaceDef(i.lineCol)
        sInterfaceDef.fullName = interfaceName
        sInterfaceDef.pkg = if (pkg.endsWith(".")) pkg.substring(0, pkg.length - 1) else pkg
        sInterfaceDef.modifiers += PUBLIC
        sInterfaceDef.modifiers += ABSTRACT

        if (i.modifiers.exists(_.modifier != "abs"))
          throw new UnexpectedTokenException(s"valid modifier for interface (abs) ${i.modifiers.toString()}", i.lineCol)

        types += interfaceName -> sInterfaceDef
        originalInterfaces += interfaceName -> i
        typeDefSet += sInterfaceDef
      }
    }

    mapOfStatements.foreach { m =>
      val imports: ListBuffer[Import] = fileNameToImport(m._1)
      val classDefs: ListBuffer[ClassStatement] = fileNameToClassDef(m._1)
      val interfaceDefs: ListBuffer[InterfaceStatement] = fileNameToInterfaceDef(m._1)
      var pkg: String = fileNameToPackageName(m._1)
      val loop = new Breaks
      loop.breakable {
        classDefs.foreach { c =>
          val sClassDef = types(pkg + c.name).asInstanceOf[SClassDef]
          var superWithoutInvocationAccess: Iterator[Access] = null
          if (c.superWithOutInvocation != null) {
            if (c.superWithOutInvocation.isEmpty) {
              sClassDef.parent =  getTypeWithName("java.lang.Object",LineCol.SYNTHETIC).asInstanceOf[SClassDef]
            }else{
              superWithoutInvocationAccess = c.superWithOutInvocation.toIterator
              val mightBeClassAccess = superWithoutInvocationAccess.next()
              val tmp = getTypeWithAccess(mightBeClassAccess,imports)
              tmp match {
                case s:SClassDef =>
                  sClassDef.parent = s
                case s:SInterfaceDef =>
                  sClassDef.superInterfaces += s
                  sClassDef.parent = getTypeWithName("java.lang.Object",c.lineCol).asInstanceOf[SClassDef]
                case _ =>
                  throw new SyntaxException(mightBeClassAccess.toString + " is not class or interface",c.lineCol)
              }
            }
          }else{

          }
        }
      }

    }


    null
  }

  def getTypeWithAccess(mightBeClassAccess: Access, imports: ListBuffer[Import]):STypeDef = {
    null
  }
  def getFieldsAndMethodsFromClass(cls: Class[_], s: STypeDef, fields: ListBuffer[SFieldDef], methods: ListBuffer[SMethodDef]):Unit = ???

  def getParameterFromClassArray(getParameterTypes: Array[Class[_]], constructorDef: SConstructorDef):Unit = ???

  def getModifierFromMember(con: Constructor[_], constructorDef: SConstructorDef):Unit = ???

  def getTypeWithName(str: String, lineCol: LineCol): STypeDef = {
    if (types.contains(str)) {
      types(str)
    } else {
      try {
        var cls = Class.forName(str)
        if (cls.isArray) {
          val name = cls.getName
          var dimension = 0
          while (cls.isArray) {
            dimension += 1
            cls = cls.getComponentType
          }
          val arrType = SArrayTypeDef()
          arrType.fullName = name
          putNameAndTypeDef(arrType, lineCol)
          arrType.dimension = dimension
          arrType.sType = getTypeWithName(cls.getName, lineCol)
          arrType
        } else {
          val modifiers: ListBuffer[SModifier] = ListBuffer()
          var typeDef: STypeDef = null
          if (cls.isAnnotation) {
            val a = SAnnoDef()
            a.fullName = str
            typeDef = a
            modifiers ++= a.modifiers
          } else if (cls.isInterface) {
            val i = SInterfaceDef(LineCol.SYNTHETIC)
            i.fullName = str
            typeDef = i
            modifiers ++= i.modifiers
          } else {
            val c = SClassDef(LineCol.SYNTHETIC)
            c.fullName = str
            modifiers ++= c.modifiers
          }
          if (cls.getPackage != null)
            typeDef.pkg = cls.getPackage.getName
          putNameAndTypeDef(typeDef, lineCol)
          getAnnotationFromAnnotatedElement(cls, typeDef)
          getModifierFromClass(cls, modifiers)

          typeDef match {
            case s: SInterfaceDef =>
              getSuperInterfaceFromClass(cls, s.superInterfaces)
              getFieldsAndMethodsFromClass(cls, s, s.fields, s.methods)
            case s: SClassDef =>
              getSuperInterfaceFromClass(cls, s.superInterfaces)
              if (cls != classOf[Object]){
                s.parent = getTypeWithName(cls.getSuperclass.getName, lineCol).asInstanceOf[SClassDef]
              }



              getFieldsAndMethodsFromClass(cls, s, s.fields,s.methods)

              for (con <- cls.getDeclaredConstructors) {
                val constructorDef = SConstructorDef(LineCol.SYNTHETIC)
                constructorDef.declaringType = s
                getAnnotationFromAnnotatedElement(con, constructorDef)
                getParameterFromClassArray(con.getParameterTypes, constructorDef)
                getModifierFromMember(con, constructorDef)
                s.constructors += constructorDef
              }
            case s: SAnnoDef =>
              for (m <- cls.getDeclaredMethods) {
                assert(m.getParameters.isEmpty)
                val annoField = new SAnnoField()
                annoField.name = m.getName
                annoField.sType = getTypeWithName(m.getReturnType.getName, lineCol)
                s.annoFields += annoField
              }
            case _ =>

          }
          typeDef
        }
      }
      catch {
        case e: Exception =>
          throw new SyntaxException("undefined class", lineCol)
      }
    }
  }


  private def getAnnotationFromAnnotatedElement(elem:AnnotatedElement,presentable:SAnnotationPresentable):Unit = {

  }

  private def getModifierFromClass(cls:Class[_],modifiers:ListBuffer[SModifier]):Unit = {

  }

  private def getSuperInterfaceFromClass(cls: Class[_], superInterfaces: ListBuffer[SInterfaceDef]) = {

  }


  private def putNameAndTypeDef(sTypeDef: STypeDef,
                                lineCol: LineCol): Unit = {
    if (types.contains(sTypeDef.fullName))
      throw new SyntaxException(s"duplicate type names ${sTypeDef.fullName}", lineCol)
    else
      types += sTypeDef.fullName -> sTypeDef
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
