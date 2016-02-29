package guide

import scala.tools.nsc.{Mode, Global}

object _15_DefaultGetterAnnotation extends App {
  val global = newGlobal()
  import global._

  object CopyAnnotationsPlugin extends global.analyzer.AnalyzerPlugin {
    val annotations = collection.mutable.AnyRefMap[Symbol, List[Tree]]()

    override def pluginsTyped(tpe: Type, typer: analyzer.Typer, tree: Tree, mode: Mode, pt: Type): Type = {
      tree match {
        case dd: DefDef if dd.symbol.isDefaultGetter =>
          // find the corresponding method that declared the default
          val methName = nme.defaultGetterToMethod(dd.name)
          val meth = typer.context.owner.info.decl(methName).filter(alt => {alt.initialize; mexists(alt.paramss)(_.hasDefault)})

          // Inspect annotations of its symbol
          if (meth.hasAnnotation(definitions.DeprecatedAttr))
            // ... and add annotations to the symbol of the default getter
            dd.symbol.addAnnotation(definitions.DeprecatedAttr)
        case _ =>
      }
      tpe
    }
  }
  global.analyzer.addAnalyzerPlugin(CopyAnnotationsPlugin)
  val tree = compile("""class C { @deprecated("", "") def foo(a: Int = 42) = a }""", global).assertNoErrors().tree
  println(tree)
  val defaultGetter = tree.collect {
    case dd: DefDef if dd.name.toString.contains("foo$default") => dd
  }.head.symbol
  assert(defaultGetter.isDeprecated, defaultGetter.defString)
}
