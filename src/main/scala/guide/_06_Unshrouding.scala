package guide

import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.{Global, Settings}

object _06_Unshrouding extends App {
  ===("Unshrouding type checking")
  p("// Q. Okay, so was that some sort of compiler builtin? Or a macro?")
  p("// A. Trace the typechecker output. The easy way would be to compile a file with -Ytyper-debug")
  p("//    But rather than heading to a terminal, lets do it the hard way")


  p("// Let's start a fully fledged compiler (rather than the toolbox compiler we used earlier)")
  val settings = new Settings()
  p("// Meaning: Use the classpath of the JVM as the classpath of the compiler, print the AST after the parser and typer phase\n, trace the internal operation of the typechecker, log macro expansions")
  settings.processArgumentString("-usejavacp -Xprint:parser,typer -Ytyper-debug -Ymacro-debug-lite")
  val reporter = new StoreReporter(settings)
  val global = new Global(settings, reporter)
  import global._
  val run = new Run
  val source = newSourceFile( """class Wrap { import scala.reflect.runtime.universe._; def test(t: Tree) = q"println($t)" }""")
  run.compileSources(source :: Nil)
  assert(reporter.infos.isEmpty, "No errors or warnings expected")

  ---------------
  p(
    """
      |Okay, the parser has desugared the string interpoloation to:
      |
      |  StringContext("println(", ")").q(t)
      |
      | Typechecking proceeds as:
      |
      |  StringContext.apply("println(", ")") // insert apply call
      |  Quasiquote(StringContext.apply(..)).q // implicit conversion to find extension method
      |  .SyntacticApplied.apply(...)         // macro expansion
      |
    """.stripMargin)

  ---------------
  p(
    """ Stray Notes:
      |
      | - -Xprint:... can be combined with -Xprint-types or various -Yshow-* options. Run `scala -Y` or `scala -X` for a list
      | - The implicit search wasn't logged. That will be improved in 2.11.8 https://github.com/scala/scala/pull/4688
      | - -Ytyper-debug logs show up after the -Xprint:typer output. This was during retypeching in the erasure phase.
      | - parser, typer, and erasure are just some of the phases you'll find in `scalac -Xprint-phases`.
    """.stripMargin)

  def next = _07_Typer
}
