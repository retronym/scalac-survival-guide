package guide

import com.sun.tools.javac.code

import scala.reflect.internal.Flags
import scala.tools.nsc.{Phase, SubComponent, Global}

object _14_SymbolTableMutation extends App {
  val g1 = newGlobal("-Xfatal-warnings")
  import g1._

  def mutateScope(owner: Symbol, f: Scope => Unit) = {
    owner.info match {
      case tc: analyzer.LockingTypeCompleter =>
        owner.setInfo(new analyzer.LockingTypeCompleter {
          override val tree = tc.tree
          override def completeImpl(sym: g1.analyzer.global.Symbol): Unit = {
            tc.completeImpl(owner)
            f(owner.info.decls)
          }
        })
      case cit: ClassInfoType =>
        f(cit.decls)
      case _ =>
    }
  }

  object AddSyntheticsPlugin extends g1.analyzer.MacroPlugin {
    override def pluginsEnterSym(namer: analyzer.Namer, tree: Tree): Boolean = {
      tree match {
        case cd: ClassDef if cd.name == TypeName("C") =>
          val ctx = namer.standardEnterSym(cd)
          val sym = namer.ensureCompanionObject(cd)
          val syntheticMember = sym.newMethodSymbol(TermName("dummy"), sym.pos).setInfo(NullaryMethodType(definitions.StringTpe))
          mutateScope(sym.moduleClass, _.enter(syntheticMember))
          true
        case _ =>
          false
      }
    }
  }
  g1.analyzer.addMacroPlugin(AddSyntheticsPlugin)
  compile("class C(fred: String); class Test { def test = C.dummy.reverse }", g1).assertNoErrors().tree
}
