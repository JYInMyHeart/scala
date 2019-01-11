package c89

class VariableSymbol(val name:String,
                     val varType:String) {
  
}
object VariableSymbol{
  def apply(name: String,
            varType: String): VariableSymbol = new VariableSymbol(name, varType)
}
