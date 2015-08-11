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
  case class State(exprMode: Boolean) {
    def adapt(tp: Type) = tp match {
      case NullaryMethodType(result) if exprMode => result
      case tp => tp
    }
    def inExprMode = copy(exprMode = true)
  }
  def typed(t: Tree) = typedTree(t, new State(false))
  private def typedTrees(ts: List[Tree], state: State) = ts map (t => typedTree(t, state))
  private def typedTree(t: Tree, state: State): Type = {
    def result = t match {
      case Select(qual, name) =>
        val qual1 = typedTree(qual, state)
        qual1.member(name) match {
          case NoSymbol => error(t)
          case member =>
            qual1.memberType(member)
        }
      case Typed(expr, tpt) =>
        val expr1 = typedTree(expr, state.inExprMode)
        val tpt1 = typedTree(tpt, state)
        if (expr1.isError) expr1
        else if (expr1 <:< tpt1) tpt1
        else error(t)
      case TypeApply(fun, args) =>
        val fun1 = typedTree(fun, state)
        val args1 = typedTrees(args, state)
        if (fun1.isError || args1.exists(_.isError)) error(t)
        else {
          val tparams = fun1.typeParams
          if (tparams.length != args.length) error(t)
          else {
            fun1.resultType.instantiateTypeParams(tparams, args1)
          }
        }
      case Apply(fun, args) =>
        val fun1 = typedTree(fun, state)
        val args1 = typedTrees(args, state)
        if (fun1.isError || args1.exists(_.isError)) error(t)
        else {
          val params = fun1.params
          if (params.length != args.length) error(t)
          else {
            if (!args1.corresponds(params)((a, p) => a <:< p.info)) error(t)
            else fun1.resultType
          }
        }
      case Literal(c : Constant) => c.tpe
      case Ident(name) => lookupIdent(g)(name).tpeHK
      case AppliedTypeTree(tpt, args) =>
        val tpt1 = typedTree(tpt, state)
        val args1 = args map (arg => typedTree(arg, state))
        if (tpt1.isError || args1.exists(_.isError)) error(t)
        else {
          val tparams = tpt1.typeParams
          if (tparams.length != args.length) error(t)
          else tpt1.instantiateTypeParams(tparams, args1)
        }
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
