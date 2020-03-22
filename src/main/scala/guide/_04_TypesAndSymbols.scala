package guide

object _04_TypesAndSymbols extends App {
  val global = newGlobal()
  new global.Run
  import global._

  def symbols = {
    ===("Types, Symbols, too)")
    p("// All declarations / definitions give rise to a Symbol.")
    val result = compile("class C { def foo = 42; def bar(x: Int) = foo }", global)
    result.tree.collect { case dt: DefTree => p(dt.symbol.defString) }

    p("// A lot of common symbols are available in definitions")
    p(List(definitions.StringClass, definitions.IntClass, definitions.Any_toString, definitions.ArrayModule))
    p("// We can also look them up by a fully qualified name")
    p(rootMirror.getClassIfDefined("java.lang.String"))
    p("// Beware the null object NoSymbol")
    p(rootMirror.getClassIfDefined("java.lang.Twine"))
    p("// symbolOf: a convenience method that lets us summon a Symbol")
    p(symbolOf[java.lang.String])
    // p(symbolOf[java.lang.Twine]) compiler error
    p("// RefTree: after typechecking, Ident, Select, and Import trees")
    p("// hold the symbol of the entity they reference")
    p(result.tree.collect { case rt: RefTree => rt })
    p("// After DefDef, ValDef, ClassDef, ModuleDef, PackageDef trees hold the symbol entity they define")
    p(result.tree.collect { case rt: DefTree => (rt.productPrefix, rt.symbol.name) })
  }
  def types = {
    p("// like symbols, many common types are available in definitions")
    p(definitions.IntTpe)
    p(definitions.IntClass.tpeHK)
    p("// This is a TypeRef, which enapsulates a prefix, an symbol, and a list of type arguments")
    p(showRaw(definitions.IntTpe))
    definitions.init()
    p("// Types can also be summoned with the `typeOf` macro")
    p(List(typeOf[Int], typeOf[List[String]], typeOf[x.type forSome { val x : String with Int }]))
    p("// Types have members")
    p(typeOf[Int].members.take(5))

  }

  symbols
  types

  def next = _05_Unshrouding
}
