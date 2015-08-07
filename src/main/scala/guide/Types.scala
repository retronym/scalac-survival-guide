package guide

object Types {
  val global = newGlobal()
  new global.Run
  import global._
  def symbols = {
    ===("Types, Symbols, too)")
    p("// All declarations / definitions give rise to a Symbol.")
    val result = compile("class C { def foo = 42; def bar = foo }", global)
    result.tree.collect { case dt: DefTree => p(dt.symbol.defString) }
    p("// A lot of common symbols are available in definitions")
    p(List(definitions.StringClass, definitions.IntClass, definitions.Any_toString, definitions.ArrayModule))
    p("// We can also look them up by a fully qualified name")
    p(rootMirror.getClassIfDefined("java.lang.String"))
    p("// Beware the null object NoSymbol")
    p(rootMirror.getClassIfDefined("java.lang.Twine"))
    p("// We can also use a convenience method that lets us summon a Symbol based on a type argument")
    p(symbolOf[java.lang.String])
    // p(symbolOf[java.lang.Twine]) compiler error
    p("")
    p("// After typechecking, Ident, Select, and Import trees hold the symbol of the entity they reference")
    p(result.tree.collect { case rt: RefTree => rt })
  }
  def types = {
    p("// like symbols, many common types are available in definitions")
    p(definitions.IntTpe)
    p("// This is a TypeRef, which enapsulates a prefix, an symbol, and a list of type arguments")
    p(showRaw(definitions.IntTpe))
    definitions.init()
    p("// Types can also be summoned with the `typeOf` macro")
    p(List(typeOf[Int], typeOf[List[String]], typeOf[x.type forSome { val x : String with Int }]))
    p("// Types have members")
    p(typeOf[Int].members.take(5))

  }

  def main(args: Array[String]) {
    symbols
    types


  }
}
