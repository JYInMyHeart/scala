package code

//import code.NutritionFacts.Builder

class NutritionFacts(val servingsSize:Int,
                     val servings:Int) {
  var calories:Int = _
  var fat:Int = _
  var sodium:Int = _
  var carbohydrate:Int = _


  def calories(value:Int):NutritionFacts = {
    calories = value
    this
  }
  def fat(value:Int):NutritionFacts = {
    fat = value
    this
  }
  def sodium(value:Int):NutritionFacts = {
    sodium = value
    this
  }
  def carbohydrate(value:Int):NutritionFacts = {
    carbohydrate = value
    this
  }

//  def this(builder: Builder) = {
//    this(builder.servingsSize,builder.servings)
//    calories = builder.calories
//    fat = builder.fat
//    sodium = builder.sodium
//    carbohydrate = builder.carbohydrate
//  }

}
object NutritionFacts{
//  class Builder{
//    var servingsSize:Int = _
//    var servings:Int = _
//    var calories:Int = _
//    var fat:Int = _
//    var sodium:Int = _
//    var carbohydrate:Int = _
//
//
//    def calories(value:Int):Builder = {
//      calories = value
//      this
//    }
//    def fat(value:Int):Builder = {
//      fat = value
//      this
//    }
//    def sodium(value:Int):Builder = {
//      sodium = value
//      this
//    }
//    def carbohydrate(value:Int):Builder = {
//      carbohydrate = value
//      this
//    }
//    def this(servingsSize:Int,servings:Int) ={
//      this
//      new NutritionFacts(servingsSize,servings)
//    }


  def main(args: Array[String]): Unit = {
    val cocaCola = new NutritionFacts(1,2)
      .calories(3)
      .carbohydrate(4)
      .fat(5)
      .sodium(7)
  }
}

