package guide

object _08_OwnGoals extends App {
  ===("Own Goals")
  def testUntil(phase: String) = {
    p(phase)
    val global = newGlobal("-Ystop-after:" + phase)
    import global._
    val result = compile( """class C { def test = assert(false, {val xxx = "".reverse; xxx}) }""", global)
    assert(!result.error, result.infos)
    result.tree.collect { case vc: ValDef if vc.name.string_==("xxx") =>
      println(vc.symbol.originalOwner)
      println(vc.symbol.ownerChain)
    }
  }
  // Finding the enclosing class or method of a symbol is a time-sensitive operation.
  // Here the owner of `xxx` starts out as `test`, but later morphs into `apply` -> $anon
  testUntil("typer")
  testUntil("uncurry")
}
