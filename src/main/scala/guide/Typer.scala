package guide

import scala.tools.nsc.Global

object Typer {
  def main(args: Array[String]) {
    val global = newGlobal()
    import global._
    val typer = new Typer[global.type](global)
    p(typer.typed(q"1 : String"))
//    p(typer.typed(q"1 : Int"))
//    p(typer.typed(q"1.toInt : Int"))
//    p(typer.typed( q"""_root_.scala.Some[String]("") : Option[String] """))
//    p(typer.typed( q"""_root_.scala.Some("") : Option[String] """))
  }
}

class Typer[G <: Global](val g: G) {
  import g._
  private def error(t: Tree) = ErrorType

  private var indent = 0
  private val debug = false
  private def trace[A](msg: String)(body: => A) = {
    def p(msg: String) = if (debug) println((" " * 4 * indent) + msg)
    p(msg)
    indent += 1
    val result = try body finally indent -= 1
    p("=> " + showRaw(result).replaceAll("\n", "; ").take(60))
    result
  }

  case class State()

  def typed(t: Tree, state: State = new State()): Type = trace(s"typeOf($t)")(
    t match {
      case _ => error(t)
    }
  )
}
