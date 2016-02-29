package guide

import scala.collection.immutable.ListMap
import scala.reflect.macros.blackbox
import scala.tools.nsc.{Global, Mode}

object InlineMacro {
  def inlineMap(c: blackbox.Context)(t: c.Tree): c.Tree = {
    import c.universe._

    val Option_map = definitions.OptionClass.info.decl(TermName("map"))
    val Option_getOrElse = definitions.OptionClass.info.decl(TermName("getOrElse"))
    val Option_isDefined = definitions.OptionClass.info.decl(TermName("isDefined"))
    val Option_exists = definitions.OptionClass.info.decl(TermName("exists"))
    val Option_get = definitions.OptionClass.info.decl(TermName("get"))
    val Some_apply = definitions.SomeModule.info.decl(TermName("apply"))
    val UncheckedBoundsClass = {
      try c.mirror.staticClass("scala.reflect.internal.annotations.uncheckedBounds")
      catch { case _: ScalaReflectionException => NoSymbol }
    }
    def uncheckedBounds(tp: Type): Type = {
      if ((tp.typeArgs.isEmpty && (tp match { case _: TypeRef => true; case _ => false}))|| UncheckedBoundsClass == NoSymbol) tp
      else withAnnotation(tp, Annotation(UncheckedBoundsClass.asType.toType, Nil, ListMap()))
    }
    def withAnnotation(tp: Type, ann: Annotation): Type = withAnnotations(tp, List(ann))

    def withAnnotations(tp: Type, anns: List[Annotation]): Type = tp match {
      case AnnotatedType(existingAnns, underlying) => internal.annotatedType(anns ::: existingAnns, underlying)
      case ExistentialType(quants, underlying) => internal.existentialType(quants, withAnnotations(underlying, anns))
      case _ => internal.annotatedType(anns, tp)
    }

    def withoutAnnotations(tp: Type): Type = tp match {
      case AnnotatedType(anns, underlying) => withoutAnnotations(underlying)
      case ExistentialType(quants, underlying) => internal.existentialType(quants, withoutAnnotations(underlying))
      case _ => tp
    }

    import c.internal._
    typingTransform(t) {
      (tree, api) =>
        api.default(tree) match {
          case Apply(TypeApply(sel @ Select(qual, _), _), (fun @ Function(param :: Nil, body)) :: Nil) if sel.symbol == Option_map =>
            val syntheticRangePos = tree.pos.makeTransparent
            val syntheticPos = tree.pos.focus

            val optionTemp: Tree = {
              val temp = internal.setInfo(internal.newTermSymbol(api.currentOwner, c.freshName("qual"), sel.pos.focus, Flag.SYNTHETIC), uncheckedBounds(qual.tpe))
              internal.setType(setPos(valDef(temp, changeOwner(qual, api.currentOwner, temp)), sel.pos.makeTransparent), NoType)
            }
            val elemType = Option_get.typeSignatureIn(qual.tpe).resultType
            val optionGetTemp: Tree = {
              val temp = internal.setInfo(internal.newTermSymbol(api.currentOwner, c.freshName("x1"), syntheticRangePos, Flag.SYNTHETIC), uncheckedBounds(elemType))
              internal.setType(valDef(temp, api.typecheck(Select(gen.mkAttributedStableRef(optionTemp.symbol), Option_get))), NoType)
            }
            val mapGet = setPos(internal.setType(Block(optionGetTemp :: Nil, substituteSymbols(changeOwner(body, fun.symbol, api.currentOwner), param.symbol :: Nil, optionGetTemp.symbol :: Nil)), tree.tpe), syntheticRangePos)
            val result = setPos(treeCopy.Block(tree,
              optionTemp :: Nil,
              internal.setType(atPos(syntheticRangePos)(If(atPos(syntheticPos)(gen.mkAttributedSelect(gen.mkAttributedStableRef(optionTemp.symbol), Option_isDefined)),
                setType(Apply(atPos(syntheticPos)(gen.mkAttributedRef(Some_apply)), mapGet :: Nil), tree.tpe),
                atPos(syntheticPos)(gen.mkAttributedIdent(definitions.NoneModule)))), tree.tpe)), syntheticRangePos)
//             println(show(tree, printPositions = true))
//             println(show(result, printPositions = true))
            result
          case Apply(sel @ Select(qual, _), (fun @ Function(param :: Nil, body)) :: Nil) if sel.symbol == Option_exists =>
            val syntheticRangePos = tree.pos.makeTransparent
            val syntheticPos = tree.pos.focus

            val optionTemp: Tree = {
              val temp = internal.setInfo(internal.newTermSymbol(api.currentOwner, c.freshName("qual"), sel.pos.focus, Flag.SYNTHETIC), uncheckedBounds(qual.tpe))
              internal.setType(setPos(valDef(temp, changeOwner(qual, api.currentOwner, temp)), sel.pos.makeTransparent), NoType)
            }
            val elemType = Option_get.typeSignatureIn(qual.tpe).resultType
            val optionGetTemp: Tree = {
              val temp = internal.setInfo(internal.newTermSymbol(api.currentOwner, c.freshName("x1"), syntheticRangePos, Flag.SYNTHETIC), uncheckedBounds(elemType))
              internal.setType(valDef(temp, api.typecheck(Select(gen.mkAttributedStableRef(optionTemp.symbol), Option_get))), NoType)
            }
            val mapGet = setPos(internal.setType(Block(optionGetTemp :: Nil, substituteSymbols(changeOwner(body, fun.symbol, api.currentOwner), param.symbol :: Nil, optionGetTemp.symbol :: Nil)), definitions.BooleanTpe), syntheticRangePos)
            val result = setPos(treeCopy.Block(tree,
              optionTemp :: Nil,
              internal.setType(atPos(syntheticRangePos)(If(atPos(syntheticPos)(gen.mkAttributedSelect(gen.mkAttributedStableRef(optionTemp.symbol), Option_isDefined)),
                mapGet,
                api.typecheck(atPos(syntheticPos)(Literal(Constant(false)))))), tree.tpe)), syntheticRangePos)
             println(show(tree, printPositions = true))
             println(show(result, printPositions = true))
            result
          case Apply(TypeApply(sel @ Select(qual, _), _), body :: Nil) if sel.symbol == Option_getOrElse =>
            val syntheticRangePos = tree.pos.makeTransparent
            val syntheticPos = tree.pos.focus

            val optionTemp: Tree = {
              val temp = internal.setInfo(internal.newTermSymbol(api.currentOwner, c.freshName("qual"), sel.pos.focus, Flag.SYNTHETIC), uncheckedBounds(tree.tpe))
              internal.setType(setPos(valDef(temp, changeOwner(qual, api.currentOwner, temp)), sel.pos.makeTransparent), NoType)
            }
            val elemType = Option_get.typeSignatureIn(qual.tpe).resultType
            val optionGetTemp: Tree = {
              val temp = internal.setInfo(internal.newTermSymbol(api.currentOwner, c.freshName("x1"), syntheticRangePos, Flag.SYNTHETIC), uncheckedBounds(elemType))
              internal.setType(valDef(temp, api.typecheck(Select(gen.mkAttributedStableRef(optionTemp.symbol), Option_get))), NoType)
            }
            val mapGet = setPos(internal.setType(Block(optionGetTemp :: Nil, gen.mkAttributedIdent(optionGetTemp.symbol)), tree.tpe), syntheticRangePos)
            val result: Tree = setPos(treeCopy.Block(tree,
              optionTemp :: Nil,
              internal.setType(atPos(syntheticRangePos)(If(atPos(syntheticPos)(gen.mkAttributedSelect(gen.mkAttributedStableRef(optionTemp.symbol), Option_isDefined)),
                mapGet,
                body)), tree.tpe)), syntheticRangePos)
//             println(show(tree, printPositions = true))
//             println(show(result, printPositions = true))
             result
          case t =>
            //sys.error(t.toString)
            t
        }
    }
  }
}


object _16_FunctionInline extends App {
  val g = newGlobal("-usejavacp -Yrangepos -Xprint:typer")
  val result = compile(
    """
      | import language.experimental._
      | class C {
      |   var x = "x"
      |   def inline[T](t: T): T = macro guide.InlineMacro.inlineMap
      |   def foo(a: Option[String]): Option[String] = inline(a.map((x: Any) => {def foo = C.this.x; foo.toString + "-" + x}))
      |   def bar(a: Option[String]): Any = inline(a.getOrElse(0))
      |   def test() = {
      |     assert(foo(Some("y")) == Some("x-y"))
      |     assert(foo(None) == None)
      |     assert(bar(Some("z")) == "z")
      |     assert(bar(None) == 0)
      |     assert(inline(Some("x").exists(_ == "x")))
      |     assert(!inline(Some("x").exists(_ == "y")))
      |     assert(!inline(None.exists(_ == true)))
      |   }
      | }""".stripMargin, g).assertNoErrors()

  result.classLoader.loadClass("C").newInstance().asInstanceOf[{def test(): Unit}].test()
}
