package tpye

import tpye.Types.{XckList, XckSymbol, XckType}

object  Environment{
  class Environment(var parent: Environment,var envs:Map[XckType,XckType]) {
    //  def environment(parent:Environment): Unit = this.parent = parent
    //  def environment(parent:Environment,envs:Map[XckType,XckType]): Unit = {
    //    this.parent = parent
    //    this.envs = envs
    //  }
    def getEnvironment(parent:Environment,binds:XckList,exprs:XckList) = {
      this.parent = parent
      envs = binds.list zip exprs.list toMap
    }

    def find(symbol:XckSymbol):Environment = {
      if(envs.contains(symbol))
        this
      else if(parent != null)
        parent.find(symbol)
      else
        throw new Exception("undefined")
    }

    def get(symbol: XckSymbol):XckType = Option.apply(find(symbol)).orNull.envs.get(symbol).orNull

    def set(symbol: XckSymbol,value:XckType):Environment = {
      envs += (symbol -> value)
      this
    }
  }
}

