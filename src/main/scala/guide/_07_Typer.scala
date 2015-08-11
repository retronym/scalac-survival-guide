package guide

import scala.tools.nsc.Global

object _07_Typer extends App {
  ===("ToyTyper")
  p("// Let's write a typechecker for Scala trees.")
  p("// How hard could it be?")

  val global = newGlobal()
  import global._
  val typer = new ToyTyper[global.type](global)
  import typer.typed
  debug = false
  p(typed(q"1"))
  p(typed(q""" "bob": Int"""))
  p(typed(q"1.toInt : Int"))
  p(typed(q"(null : Some[String]).get : String"))
  p(typed(q"(Some.apply[String](null)).get : String"))
}

class ToyTyper[G <: Global](val g: G) {
  import g._
  case class State() {
    def adapt(tp: Type) = tp
  }
  def typed(t: Tree) = typedTree(t, new State())
  private def typedTrees(ts: List[Tree], state: State) = ts map (t => typedTree(t, state))

  private def typedTree(t: Tree, state: State): Type = {
    def result = t match {
      case _ => notImplemented(t)
    }
    trace(s"typed($t, $state)") {
      state.adapt(result)
    }
  }

  private def error(t: Tree) = ErrorType
  private def notImplemented(t: Tree) =
    sys.error(s"Typechecking of ${t.productPrefix} not implemented. ${showRaw(t)}")
}
