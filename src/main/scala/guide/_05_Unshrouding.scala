package guide

object _05_Unshrouding extends App {
  ===("Unshrouding expressions")
  p("// There a a few handy ways to see what code means")
  p("// Q. What does q\"\"\"println($tree)\"\"\" mean?")
  ---------------
  p("// A. Typecheck it and find out")

  val cm = scala.reflect.runtime.currentMirror

  import cm.universe._

  import scala.tools.reflect.ToolBox

  val toolbox = cm.mkToolBox()

  def tree = toolbox.parse(
    """
      |import scala.reflect.runtime.universe._
      |def quoteMe(tree: Tree) = {
      |  q"println($tree)"
      |}
    """.stripMargin)

  p(show(toolbox.typecheck(tree)))

  () => _06_Unshrouding
}
