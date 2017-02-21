package guide

import scala.tools.nsc.transform.{InfoTransform, TypingTransformers}
import scala.tools.nsc.Global

object _22_InfoTransform extends App {

  def mkPhase(g: Global) = new AddClassName { val global: g.type = g }

  val g1 = newGlobal("-Xprint:addClassName", extraPhases = g2 => (mkPhase(g2), "addClassName") :: Nil)
  compile("class C; class D extends C; class E extends D", g1)
  compile("class D extends C; class E extends D; class C", g1)
}

abstract class AddClassName extends InfoTransform with TypingTransformers {
  import global._
  private lazy val nme_ClassName = TermName("className")
  override def transformInfo(sym: Symbol, tpe: Type): Type = {
    if (currentRun.compiles(sym) && sym.isClass) {
      tpe match {
        case ClassInfoType(parents, decls, clazz) =>
          val newScope = decls.cloneScope
          val foo = newScope.enter(sym.newMethodSymbol(nme_ClassName, sym.pos).setInfo(NullaryMethodType(definitions.StringTpe)))
          val newTpe = ClassInfoType(parents, newScope, clazz)
          exitingPhase(currentRun.phaseNamed(phaseName)) {
            val isOverride = tpe.baseClasses.drop(1).exists(bc => foo.overriddenSymbol(bc) != NoSymbol)
            if (isOverride)
              foo.setFlag(Flag.OVERRIDE)
          }
          newTpe
        case _ => tpe
      }
    } else tpe
  }
  override protected def newTransformer(unit: CompilationUnit): Transformer = new TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree match {
      case Template(parents, self, body) =>
        val classSym = tree.symbol.enclClass
        val foo = exitingPhase(currentRun.phaseNamed(phaseName))(classSym.info.decl(nme_ClassName))
        if (foo != NoSymbol) {
          val fooDef = localTyper.typedPos(tree.pos)(DefDef(foo, Literal(Constant(classSym.name.decoded))))
          treeCopy.Template(tree, transformTrees(parents), transformValDef(self), transformTrees(body) :+ fooDef)
        } else super.transform(tree)
      case _ => super.transform(tree)
    }
  }
  override val phaseName: String = "addClassName"
  override val runsAfter: List[String] = List("typer")
  override val runsRightAfter: Option[String] = None
}
