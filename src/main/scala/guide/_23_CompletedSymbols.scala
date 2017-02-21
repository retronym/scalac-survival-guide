package guide

import scala.tools.nsc.transform.{InfoTransform, TypingTransformers}
import scala.tools.nsc.Global

object _23_CompletedSymbols extends App {

  def mkPhase(g: Global) = new AddClassName { val global: g.type = g }
  val g1 = newGlobal()

  import g1._
  def safeInfo(sym: Symbol): Type = if (sym.hasRawInfo && sym.rawInfo.isComplete) sym.info else NoType
  def packageClassOrSelf(sym: Symbol): Symbol = if (sym.hasPackageFlag && !sym.isModuleClass) sym.moduleClass else sym
  def walkTopLevels(root: Symbol): List[Symbol] =
    if (root.hasPackageFlag)
      safeInfo(packageClassOrSelf(root)).decls.toList.flatMap(x =>
        if (x == root) Nil else walkTopLevels(x)
      ) else List(root)
  compile("class C { println(0.toString) }", g1)
  val completedTopLevels = walkTopLevels(RootClass).filterNot(_.hasPackageFlag).filter(x => x.associatedFile != reflect.io.NoAbstractFile && x.hasRawInfo && x.rawInfo.isComplete)
  println(completedTopLevels.filter(_.sourceFile == null).map(x => (x.fullName, x.associatedFile)).mkString("\n"))
}

