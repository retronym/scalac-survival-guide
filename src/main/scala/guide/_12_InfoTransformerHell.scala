package guide

object _12_InfoTransformerHell extends App {
  val g = newGlobal("-Ystop-after:delambdafy -Ydelambdafy:method")
  import g._

  val tree = compile("class VC(val a: Any) extends AnyVal; class D { def foo = (x: VC) => x }", g).assertNoErrors().tree
  println(show(tree))
  val sym = tree.collect {
    case dd: DefDef if dd.name.string_==("$anonfun") =>
      dd.symbol
  }.head
  println(exitingUncurry(sym.info.params.map(_.defString))) // List(x: VC)
  println(exitingPostErasure(exitingUncurry(sym.info.params).map(_.defString))) // List(x: Object)

}
