package guide

import scala.reflect.internal.util.{BatchSourceFile, Position, SourceFile}
import scala.reflect.io.PlainFile
import scala.tools.nsc

import scala.tools.nsc.reporters.StoreReporter

object _11_RunScalaRun extends App {
  val global1 = newGlobal()
//  val source1 =
//    """
//      |package object foo { implicit class Run(s: String) extends AnyVal { def run: Unit = ???  }}
//    """.stripMargin
//  def sourceFile1 = global1.newSourceFile(source1)
//  global1.currentRun.compileSources(sourceFile1 :: Nil)

  {
    val global2 = newInteractiveGlobal("-uniqid -Ystop-after:typer -classpath " + global1.settings.outdir.value)
    import global2._
    val foo = new BatchSourceFile("Foo.scala", "import scala.concurrent.duration._; class Test { 5.millis}")
    val foo2 = new BatchSourceFile("Foo2.scala", "import scala.concurrent.duration._; class Test { 5.millis}")


    def loadedTyped(source: SourceFile) {
      val typedResponse1 = new Response[Tree]
      askLoadedTyped(source, typedResponse1)
      val tree = typedResponse1.get.left
      assert(!tree.exists(_.isErroneous), tree)
      println("loadedTyped: " + tree)
    }

    /*
    (0)  = {scala.Tuple2@18873} "(method millis,DurationConversions.scala)"
(1)  = {scala.Tuple2@18877} "(class DurationInt,package.scala)"
(2)  = {scala.Tuple2@18880} "(trait DurationConversions,DurationConversions.scala)"
(3)  = {scala.Tuple2@18882} "(class AnyVal,AnyVal.scala)"
     */
    def docComment(): Unit = {
      def newSource(path: String) = new BatchSourceFile(nsc.io.AbstractFile.getFile("/code/scala/" + path))
      val DurationConversions_scala = newSource("src/library/scala/concurrent/duration/DurationConversions.scala")
      val duration_package_scala = newSource("src/library/scala/concurrent/duration/package.scala")
      val AnyVal_scala = newSource("src/library/scala/AnyVal.scala")
      val response = new Response[(String, String, Position)]
      val DurationConversionsClass = symbolOf[scala.concurrent.duration.DurationConversions]
      val DurationConversions_millis = typeOf[scala.concurrent.duration.DurationConversions].member(TermName("millis")).filter(_.typeParams.isEmpty)
      val durationPackageObject = symbolOf[scala.concurrent.duration.`package`.type]
      val durationPackageObject_DurationIntClass = durationPackageObject.info.member(TypeName("DurationInt"))
      val fragments = List(
        DurationConversions_millis -> DurationConversions_scala,
        durationPackageObject_DurationIntClass -> duration_package_scala,
        DurationConversionsClass -> DurationConversions_scala,
        symbolOf[AnyVal] -> DurationConversions_scala
      )
      askDocComment(DurationConversions_millis, DurationConversions_scala, DurationConversions_millis, fragments, response)
      println("docComment: " + response.get)
    }

    def reload(source: SourceFile): Unit = {
      val reloadResponse = new Response[Unit]
      askReload(source :: Nil, reloadResponse)
      println("reload: " + reloadResponse.get)
    }
    def typeAt(source: SourceFile): Unit = {
      val response = new Response[Tree]
      askTypeAt(source.position(source.content.length - 2), response)
      println("typeAt: " + response.get)
    }

    loadedTyped(foo)
    typeAt(foo)
    docComment()
    currentRun.asInstanceOf[TyperRun].typeCheck(unit)
//    typeAt(foo)
//    docComment()  
//    typeAt(foo)
//    reload(foo)
//    docComment()
//    typeAt(foo)
//    reload(foo)
//    loadedTyped(foo)
//    loadedTyped(foo)
//    loadedTyped(foo2)
//    loadedTyped(foo)
//    loadedTyped(foo2)
//    askShutdown()
  }
}
/*
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
<function0>
reload List(HelloSlick.scala)
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
typeat HelloSlick.scala [75:81]
wait loaded & typed HelloSlick.scala
typeat HelloSlick.scala [73:81]
doc comment method millis in DurationConversions.scala with fragments:((method millis,DurationConversions.scala),(class DurationInt,package.scala),(trait DurationConversions,DurationConversions.scala),(class AnyVal,AnyVal.scala))
typeat HelloSlick.scala [75:81]
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala
wait loaded & typed HelloSlick.scala

 */
