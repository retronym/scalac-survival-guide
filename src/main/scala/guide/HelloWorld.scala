package guide
/** Basic introduction to trees via runtime reflection and the toolbox compiler */
object HelloWorld {

  def main(args: Array[String]){
    val cm = scala.reflect.runtime.currentMirror
    import cm.universe._

    ===("It starts with a tree.")
    // println("hello world")
    def tree = Apply(Ident(TermName("println")), Literal(Constant("hello world")) :: Nil)

    p("// showRaw: print the raw representation of a tree")
    p(showRaw(tree))
    p("// show: view as something like source code")
    p(show(tree))
    p("// showCode: source code that could be reparsed")
    p(showCode(tree))

    ===("Tree, meet types")
    import scala.tools.reflect.ToolBox
    val toolbox = cm.mkToolBox()
    val typedTree = toolbox.typecheck(tree)
    p(show(typedTree, printTypes = true))

    ===("Typecheck + code generation + evaluation")

    toolbox.eval(tree)
  }
}

