package guide

import language.experimental._
import scala.collection.mutable
import scala.tools.nsc.transform.TypingTransformers

object _20_RelatedMethodAdder extends App {
  val g = newGlobal("-usejavacp -Xprint:typer -Ystop-after:typer -uniqid -Xprint-types")
  import g._
   val code = "class C { def foo[T](a: String, t: T): T = {a.reverse; t}}"

  val result = compile(code, g).assertNoErrors()
  val tree = result.tree
  val unit = result.unit

  // transform the above into:
  //
  // "class C { def foo[T](a: String, t: T): T = foo$impl(0, a, t); def foo$impl(x$1: Int, a: String, t: T): T = {x$1; { a.reverse; t } } }"
  //
  // a hopefully instructive example for @wheaties: https://gitter.im/scala/scala?at=57158a655cd40114649c745d

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
        // clone the symbol. the clone which will give us its own set of symbols for type/value parameters
        val newMethod: Symbol = tree.symbol.cloneSymbol(tree.symbol.owner, Flag.PRIVATE | Flag.ARTIFACT, TermName("foo$impl"))

        // create a symbol for the extra parameter we want to add
        val synthParamName = TermName("x$0")
        val newParam = newMethod.newSyntheticValueParam(definitions.IntTpe, synthParamName)

        // and modify the method's info to include this parameter
        newMethod.modifyInfo {
          case GenPolyType(tparams, MethodType(params, res)) => GenPolyType(tparams, MethodType(newParam :: params, res))
          case _ => ???
        }
        localTyper.namer.enterInScope(newMethod)

        // modify the RHS of the source method to forward to the synthetic one
        // we need to use `paramToArg` to make sure that vararg params are passed along with `p : _*`
        val params1: List[Tree] = localTyper.typed(Literal(Constant(0))) :: vparams.map(p => gen.paramToArg(p.symbol))
        val forwarderTree = (Apply(gen.mkAttributedRef(tree.symbol.owner.thisType, newMethod), params1) /: vparamss)((fn, vparams) => Apply(fn, vparams map gen.paramToArg))
        val forwarder = deriveDefDef(tree)(_ => localTyper.typedPos(tree.symbol.pos)(forwarderTree))
        val origTparams = tree.symbol.info.typeParams

        // References to method type parameters in the RHS will refer to method type param skolems, rather
        // than directly to the method type parameter. Find these to prepare to substitute them with references
        // the the type params of the synthetic method
        val (oldSkolems, deskolemized) = {
          origTparams match {
            case Nil => (Nil, Nil)
            case _ =>
              val skolemSubst = mutable.Map[Symbol, Symbol]()
              rhs.foreach(_.tpe.foreach {
                case tp if tp.typeSymbolDirect.isSkolem =>
                  val tparam = tp.typeSymbolDirect.deSkolemize
                  if (!skolemSubst.contains(tparam) && origTparams.contains(tparam)) {
                    skolemSubst(tp.typeSymbolDirect) = newMethod.typeParams(origTparams.indexOf(tparam))
                  }
                case _ =>
              })
              skolemSubst.toList.unzip
          }
        }

        val old = oldSkolems ::: tree.symbol.typeParams ::: allvparams.flatMap(_.map(_.symbol))
        val neww = deskolemized ::: newMethod.typeParams ::: newMethod.info.paramss.flatten.drop(1)

        val newRhs = super.transform(rhs)
          .changeOwner((tree.symbol, newMethod))
          .substituteSymbols(old, neww)
        // create the tree for our synthetic method, based on the symbol we constructed above.
        // we take the RHS of the source method and substitute in the new method as the owner
        // of any symbols within the RHS that used to be owned by the source method.
        // similarly, we substitute references to the value/type parameters of the source method
        // with the corresponding params of the synthetic method
        val newMethodTree: Tree =
          localTyper.typedPos(tree.symbol.pos)(DefDef(newMethod, Block(Ident(synthParamName), newRhs)))

        // "thicket encoding", will be flattened into the template body be the other case of this transform.
        Block(newMethodTree :: forwarder :: Nil, EmptyTree)
      case _ => super.transform(tree)
    }
  }
  val tree1 = transformer.transform(tree)
  println(showCode(tree1))
}

object Test {

}