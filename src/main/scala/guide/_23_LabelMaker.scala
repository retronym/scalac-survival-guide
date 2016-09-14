package guide

import scala.tools.nsc.Global
import scala.tools.nsc.transform.{Transform, TypingTransformers}

object _23_LabelMaker extends App {

  def mkPhase(g: Global) = new LabelMaker { val global: g.type = g }

  val g1 = newGlobal("-Xprint:labelMaker -uniqid", extraPhases = g2 => (mkPhase(g2), "labelMaker") :: Nil)
  compile("class C { def foo(x: Int) = { var y = 42; def bar(y: Int) = { x + y } }}", g1)
}


abstract class LabelMaker extends Transform with TypingTransformers {
  import global._
  override protected def newTransformer(unit: CompilationUnit): Transformer = new TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = super.transform(tree) match {
      case dd: DefDef if !dd.symbol.isConstructor =>
        val params = dd.vparamss.head.map(_.symbol)
        val labelDefSym = dd.symbol.owner.newLabel(dd.name.prepend("_"), dd.pos)
        val labelParams = params.map(p => p.cloneSymbol(labelDefSym))
        labelDefSym.setInfo(MethodType(labelParams, dd.symbol.info.resultType))
        val labelDef = LabelDef(labelDefSym, labelParams, dd.rhs.substituteSymbols(params, labelParams))//.changeOwner(dd.symbol -> labelDefSym)) // TODO changeOwner?
        val labelApply = Apply(Ident(labelDefSym), params.map(gen.mkAttributedStableRef))
        val newRhs = Block(labelDef :: Nil, labelApply)
        deriveDefDef(dd)(_ => localTyper.typedPos(dd.rhs.pos)(newRhs))
      case t => t
    }
  }
  override val phaseName: String = "labelMaker"
  override val runsAfter: List[String] = List("uncurry")
  override val runsRightAfter: Option[String] = None
}

