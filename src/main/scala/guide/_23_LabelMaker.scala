package guide

import java.nio.file.{Files, Paths}

import scala.tools.nsc.Global
import scala.tools.nsc.transform.{Transform, TypingTransformers}

object _23_LabelMaker extends App {

  def mkPhase(g: Global) = new LabelMaker { val global: g.type = g }

  val g1 = newGlobal("-Ygen-asmp /tmp -Xprint:labelMaker -uniqid", extraPhases = g2 => (mkPhase(g2), "labelMaker") :: Nil)
  val result = compile("class C { println(foo(1)); def foo(x: Int) = { var y = 42; def bar(y: Int) = { x + y } ; bar(x)}}", g1).assertNoErrors()
  println(new String(Files.readAllBytes(Paths.get("/tmp/C.asmp"))))
  result.classLoader.loadClass("C").newInstance()
}


abstract class LabelMaker extends Transform with TypingTransformers {
  import global._
  class ShallowSubstitutor(owner: Symbol, from: List[Symbol], to: List[Symbol]) extends TreeSymSubstituter(from, to) {
    override def transform(tree: Tree): Tree = if (currentOwner != owner) tree else super.transform(tree)
  }
  override protected def newTransformer(unit: CompilationUnit): Transformer = new TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree match {
      case dd: DefDef if !dd.symbol.isConstructor =>
        val params = dd.vparamss.head.map(_.symbol)
        val labelDefSym = dd.symbol.owner.newLabel(dd.name.prepend("_"), dd.pos)
        val labelParams = params.map(p => p.cloneSymbol(newOwner = NoSymbol))
        labelDefSym.setInfo(MethodType(labelParams, definitions.UnitTpe))
        val labelRhs: Tree = {
          val subst = new ShallowSubstitutor(currentOwner, params, labelParams)
          subst.atOwner(currentOwner) {
            subst(super.transform(dd.rhs))
          }
        }
        val labelDef = LabelDef(labelDefSym, labelParams, labelRhs)//.changeOwner(dd.symbol -> labelDefSym)) // TODO changeOwner?
        val labelApply = Apply(Ident(labelDefSym), params.map(gen.mkAttributedStableRef))
        val newRhs = Block(labelDef :: Nil, labelApply)
        deriveDefDef(dd)(_ => localTyper.typedPos(dd.rhs.pos)(newRhs))
      case t => super.transform(tree)
    }
  }
  override val phaseName: String = "labelMaker"
  override val runsAfter: List[String] = List("uncurry")
  override val runsRightAfter: Option[String] = None
}

