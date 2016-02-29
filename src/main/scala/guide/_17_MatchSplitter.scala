package guide

import scala.reflect.macros.blackbox
import language.experimental._


/**
  * Split `match { case _=>`
  */
object MatchSplitter {
  def splitMatchBody(c: blackbox.Context)(t: c.Tree): c.Tree = {
    import c.universe._
    import c.internal._

    typingTransform(t) {
      (tree, api) =>
        tree match {
          case Match(sel, cases) =>
            def splitCase(cdef: CaseDef): CaseDef = {
              val sym: Symbol = internal.setInfo(newMethodSymbol(api.currentOwner, TermName("caseBody"), cdef.body.pos.focus), nullaryMethodType(cdef.tpe))
              val caseBodyDef = atPos(cdef.pos.focus)(setType(defDef(sym, api.default(cdef.body)), NoType))
              treeCopy.CaseDef(cdef, api.default(cdef.pat), api.default(cdef.guard), treeCopy.Block(cdef, caseBodyDef :: Nil, atPos(cdef.body.pos.focus)(gen.mkAttributedIdent(sym))))
            }
            treeCopy.Match(tree, sel, cases.map(splitCase(_)))
          case t => t
        }
    }
  }
  def split[T](t: T): T = macro guide.MatchSplitter.splitMatchBody
}


object _17_MatchSplitter extends App {
  val g = newGlobal("-usejavacp -Xprint:typer")
  val result = compile(
    """
      | import guide.MatchSplitter.split
      | class C {
      |   def x = 42
      |   def test() = {
      |     val x = split("" match { case "a" => 12; case "" => def foo = C.this.x; foo })
      |     assert(x == 42)
      |   }
      | }""".stripMargin, g).assertNoErrors()
  /*
      def test(): Unit = {
      val x: Int = ("" match {
        case "a" => {
          def caseBody: Int(12) = 12;
          caseBody
        }
        case "" => {
          def caseBody: Int = {
            def foo: Int = C.this.x;
            foo
          };
          caseBody
        }
      }: Int);
   */

  result.classLoader.loadClass("C").newInstance().asInstanceOf[{def test(): Unit}].test()
}
