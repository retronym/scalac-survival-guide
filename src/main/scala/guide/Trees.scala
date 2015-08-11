package guide

object Trees {

   def main(args: Array[String]){
     val cm = scala.reflect.runtime.currentMirror
     import cm.universe._
     ===("More ways to create trees")
     import scala.tools.reflect.ToolBox
     val toolbox = cm.mkToolBox(options = "-uniqid")

     p("// We can also parse source code into a tree")
     def parsedTree = toolbox.parse("""println("hello parser")""")
     toolbox.eval(parsedTree)

     def quotedTree: Tree = q"""println("hello quasiquote")"""
     toolbox.eval(quotedTree)

     p("// This is called quasiquoting. Composing trees by splicing them into quasiquotes")
     def splicedTree: Tree = q"""println( { $quotedTree; $quotedTree; "okay"} )"""
     p(show(splicedTree))

     p("// Wait, what was that q doing?")
     p("// We'll dive into that one shortly!")

     p("// The structure of trees can be explored with a few combinators, like in the collections:")
     p("// foreach ")
     parsedTree.foreach(_ => ())
     p("// collect ")
     p(parsedTree.collect { case Ident(name) => name })
     val typed = toolbox.typecheck(parsedTree)

     val printlns = typeOf[Predef.type].member(TermName("println")).alternatives
     assert(printlns.length == 2, printlns)
     val hasPrintln = typed.exists {
       case q"$qual(...$args)" =>
         printlns contains qual.symbol
       case _ => false
     }
     p(s"$typed does ${if (hasPrintln) "" else "not "}call println")

     p("// For more control over the recursion, use a Traverser")
     p("// ... and, to generate a new tree, use a Transformer")
   }
 }
