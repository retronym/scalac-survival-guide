package guide

object _10_Mixin extends App {
  ===("Mixin")
    val global = newGlobal("-Ystop-after:mixin")
    import global._
    val result = compile( """trait T { object X }; class C extends T""", global)
    assert(!result.error, result.infos)
    result.tree.collect {
      case vc: DefDef if vc.name.string_==("X") =>
        println(vc.symbol.fullLocationString)
        println(vc.symbol.defString)
        println(vc.symbol.debugFlagString)
    }
}
