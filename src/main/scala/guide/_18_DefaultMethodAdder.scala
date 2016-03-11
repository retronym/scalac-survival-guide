package guide

import java.nio.Buffer

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.macros.blackbox
import language.experimental._

object _18_DefaultMethodAdder extends App {
  val g = newGlobal("-usejavacp -Xprint:typer")
  import g._
  val unit = newCompilationUnit("class C { @unchecked def foo(a: Int = 0) = 0 }")
  val parsed = newUnitParser(unit).parse()
  object transformer extends Transformer {
    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      val flattened = stats.flatMap {
        case Block(stats, EmptyTree) => stats
        case x => x :: Nil
      }
      super.transformStats(flattened, exprOwner)
    }
    override def transform(tree: Tree): Tree = tree match {
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) if mods.annotations.exists(_.toString.contains("new unchecked")) =>
        val siblings = mutable.Buffer[Tree]()
        foreachWithIndex(vparamss.flatten) { (tree, i) =>
          tree match {
          case vd @ ValDef(_, _, _, EmptyTree) =>
          case _ =>
            siblings += DefDef(mods, nme.defaultGetterName(name, i).append("_sibling"), Nil, Nil, EmptyTree, gen.mkAttributedRef(currentRun.runDefinitions.Predef_???))
          }
        }
        siblings.toList match {
          case Nil => tree
          case xs => Block(super.transform(tree) :: xs, EmptyTree)
        }
      case _ => super.transform(tree)
    }
  }
  val tree1 = transformer.transform(parsed)
  println(showCode(tree1))
}
