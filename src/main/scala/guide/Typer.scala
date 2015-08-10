package guide

import scala.tools.nsc.Global

object Typer {
  def main(args: Array[String]) {
    val global = newGlobal()
    import global._
    val typer = new Typer[global.type](global)
    p(typer.typed(q"1"))
    p(typer.typed(q""" "bob": Int"""))
    p(typer.typed(q"1.toInt : Int"))
    p(typer.typed(q"(null : Some[String]).get : String"))
    //    p(typer.typed(q"1 : Int"))
//    p(typer.typed( q"""_root_.scala.Some[String]("") : Option[String] """))
//    p(typer.typed( q"""_root_.scala.Some("") : Option[String] """))
  }
}

class Typer[G <: Global](val g: G) {
  import g._
  case class State(exprMode: Boolean = true) {
    def inExprMode = copy(exprMode = true)
    def adapt(tp: Type) = tp match {
      case NullaryMethodType(result) if exprMode => result
      case tp => tp
    }
  }
  def typed(t: Tree, state: State = new State()): Type = trace(s"typeOf($t)") {
    def typed1(tree: Tree) = typed(tree, state)
    val result = t match {
      case Select(qual, name) =>
        val qual1 = typed1(qual)
        qual1.member(name) match {
          case NoSymbol => error(t)
          case member =>
            qual1.memberType(member)
        }
      case Typed(expr, tpt) =>
        val expr1 = typed1(expr)
        val tpt1 = typed1(tpt)
        if (expr1.isError) expr1
        else if (expr1 <:< tpt1) tpt1
        else error(t)
      case Literal(c : Constant) => c.tpe
      case Ident(name) =>
        val context = analyzer.newTyper(analyzer.rootContext(NoCompilationUnit)).context
        val lookup = context.lookupSymbol(name, _ => true)
        lookup.symbol.tpeHK
      case AppliedTypeTree(tpt, args) =>
        val tpt1 = typed1(tpt)
        val args1 = args map typed1
        if (tpt1.isError || args1.exists(_.isError)) error(t)
        else {
          val tparams = tpt1.typeParams
          if (tparams.length != args.length) error(t)
          else tpt1.instantiateTypeParams(tparams, args1)
        }
      case _ => notImplemented(t)
    }
    state.adapt(result)
  }

  var indent = 0
  val debug = false
  def trace(msg: String)(body: => Type): Type = {
    def p(msg: String) = if (debug) println((" " * 4 * indent) + msg)
    p(msg)
    indent += 1
    val result = try body finally indent -= 1
    p("=> " + show(result).replaceAll("""\{\n"""", "{ ").replaceAll("\n", "; ").take(60))
    result
  }

  private def error(t: Tree) = ErrorType
  private def notImplemented(t: Tree) =
    sys.error(s"Typechecking of ${t.productPrefix} not implemented. ${showRaw(t)}")
}
