package guide

import language.experimental._
import scala.tools.nsc.transform.TypingTransformers

object _20_RelatedMethodAdder extends App {
  val g = newGlobal("-usejavacp -Xprint:typer -Ystop-after:typer -uniqid -Xprint-types")
  import g._
  val code = "class C { def foo[T](a: String, t: T): T = {a.reverse; t; ???} }"

  // val code = "class C { def foo[T](a: String, t: T): T = {a.reverse; t; ???} }"
  // ^-- fails, probably related to method type param skolems could just add some casts.

  val result = compile("class C { def foo[T](a: String, t: T): T = {a.reverse; t; ???} }", g).assertNoErrors()
  val tree = result.tree
  val unit = result.unit


  object typingTransform extends TypingTransformers {
    override val global: g.type = g
  }
  object transformer extends typingTransform.TypingTransformer(unit) {
    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      val flattened = stats.flatMap {
        case Block(stats, EmptyTree) => stats
        case x => x :: Nil
      }
      super.transformStats(flattened, exprOwner)
    }
    override def transform(tree: Tree): Tree = tree match {
      case DefDef(mods, name, tparams, allvparams @ (vparams :: vparamss), tpt, rhs) if name.string_==("foo") =>
        val newMethod: Symbol = tree.symbol.cloneSymbol(tree.symbol.owner, Flag.PRIVATE | Flag.ARTIFACT, TermName("foo$impl"))
        val synthParamName = TermName("x$0")
        val newParam = newMethod.newSyntheticValueParam(definitions.IntTpe, synthParamName)
        newMethod.modifyInfo {
          case GenPolyType(tparams, MethodType(params, res)) => GenPolyType(tparams, MethodType(newParam :: params, res))
          case _ => ???
        }
        val params1: List[Tree] = localTyper.typed(Literal(Constant(0))) :: vparams.map(p => gen.paramToArg(p.symbol))
        val forwarderTree = (Apply(gen.mkAttributedRef(tree.symbol.owner.thisType, newMethod), params1) /: vparamss)((fn, vparams) => Apply(fn, vparams map gen.paramToArg))
        val forwarder = deriveDefDef(tree)(_ => localTyper.typedPos(tree.symbol.pos)(forwarderTree))
        val newMethodTree: Tree =
          localTyper.typedPos(tree.symbol.pos)(DefDef(newMethod, Block(Ident(synthParamName),
            super.transform(rhs)
              .changeOwner((tree.symbol, newMethod))
              .substituteSymbols(tree.symbol.typeParams ::: allvparams.flatMap(_.map(_.symbol)), newMethod.typeParams ::: newMethod.info.paramss.flatten.drop(1)))))
        Block(newMethodTree :: forwarder :: Nil, EmptyTree)
      case _ => super.transform(tree)
    }
  }
  val tree1 = transformer.transform(tree)
  println(showCode(tree1))
}

