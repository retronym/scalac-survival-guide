package guide

import scala.tools.nsc.Global
import scala.tools.nsc.transform.TypingTransformers

object _21_ParamAlias extends App {
  def run(g: Global)(unit: g.CompilationUnit): Unit = {
    object transformFields extends TypingTransformers {
      override val global: g.type = g
      import global._
      object trans extends TypingTransformer(unit) {
        override def transform(tree: Tree): Tree = tree match {
          case vd: ValDef if vd.symbol.owner.isClass =>
            val getter = vd.symbol.getterIn(vd.symbol.owner)
            if (getter.isParamAccessor) {
              if (vd.symbol.alias != NoSymbol) {
                println(s"Eliding related method for ${vd.symbol.fullLocationString}, as it is an alias for ${vd.symbol.alias.fullLocationString} / ${vd.symbol.alias.getterIn(vd.symbol.alias.owner).defString}")
                super.transform(vd)
              } else {
                val newMethodTree = {
                  val sym = currentOwner.newMethodSymbol(vd.name.dropLocal + "$extraMethod", vd.symbol.pos, Flag.SYNTHETIC | Flag.STABLE).setInfo(NullaryMethodType(vd.symbol.info))
                  DefDef(sym, gen.mkAttributedRef(vd.symbol))
                }
                Block(super.transform(vd) :: localTyper.typedPos(vd.pos)(newMethodTree) :: Nil, EmptyTree)
              }
            } else super.transform(vd)
          case _ => super.transform (tree)
        }
        override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
          super.transformStats(stats, exprOwner).flatMap {
            case Block(stats, EmptyTree) => stats
            case x => x :: Nil
          }
        }

      }

    }
    unit.body = transformFields.trans.transform(unit.body)
  }

  val g1 = newGlobal("-Xprint:all", extraPhases = g2 => newSubcomponent(g2, "typer")((g3, unit) => run(g3)(unit)) :: Nil)
  val tree = compile("class C(val i: Int); class D(override val i: Int) extends C(i)", g1).assertNoErrors().tree
  import g1._
  print(show(tree, printIds = true))
  /*
  package <empty> {
  class C extends Object {
    <paramaccessor> private[this] val i: Int = _;
    <synthetic> <stable> def i$info(): Int = C.this.i;
    <stable> <accessor> <paramaccessor> def i(): Int = C.this.i;
    def <init>(i: Int): C = {
      C.this.i = i;
      C.super.<init>();
      ()
    }
  };
  class D extends C {
    override <stable> <accessor> <paramaccessor> def i(): Int = (D.super.i(): Int);
    def <init>(i: Int): D = {
      D.super.<init>(i);
      ()
    }
  }
}
   */
}
