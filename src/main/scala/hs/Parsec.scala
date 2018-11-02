package hs

class Parsec {
  type Line = Int
  type Column = Int

  case class Pos(var line:Line,var column:Column)

  object Pos{
    def apply( line: Line, column: Column): Pos =
      new Pos( line, column)
  }


  def updatePos(pos:Pos,c:Char):Pos = c match {
    case '\n' =>  Pos(pos.line + 1,1)
    case '\t' =>  Pos(pos.line, (pos.column + 8 - (pos.column - 1)) % 8)
    case _ =>  Pos(pos.line,pos.column + 1)
  }

  def initialPos:Pos = Pos(1,1)

  case class State[+A](stateInput:State[A],statePos:Pos)


}
