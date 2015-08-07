package guide

object AlternativeForestry {

   def main(args: Array[String]){
     val cm = scala.reflect.runtime.currentMirror
     import cm.universe._
     ===("Alternative ways ")
     p("// We can also parse source code into a tree")
     import scala.tools.reflect.ToolBox
     val toolbox = cm.mkToolBox()
     def parsedTree = toolbox.parse("""println("hello parser")""")
     toolbox.eval(parsedTree)

     def quotedTree: Tree = q"""println("hello quasiquote")"""
     toolbox.eval(quotedTree)
     p("// This is called quasiquoting. Composing trees by splicing them into quasiquotes")
     def splicedTree: Tree = q"""println( { $quotedTree; $quotedTree; "okay"} )"""
     p(show(splicedTree))

     p("// Wait, what was that q doing?")
     p("// Let's go fishing")
   }
 }
