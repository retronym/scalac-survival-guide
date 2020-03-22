import scala.annotation.elidable
import scala.reflect.internal.util.ScalaClassLoader
import scala.reflect.io.File
import scala.tools.nsc.{Global, Phase, Settings, SubComponent}
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.reflect.ReflectMain._

package object guide {
  val Width = 80
  def --------------- = println("\n" + ("-" * Width) + "\n")

  def ===(s: String) = p(title('=', s))
  def ---(s: String) = p(title('-', s))

  def title(filler: Char, s: String) = {
    val diff = Width - s.length - 1
    val l = diff / 2
    val r = diff - l
    "\n" + (filler.toString * l) + " " + s + " " + (filler.toString * r) + "\n"
  }

  var lastWasComment = false
  def p(a: Any) = {
    val msg = a.toString
    val isComment = msg.startsWith("// ")
    if (lastWasComment) println(msg)
    else println("\n" + msg)
    lastWasComment = isComment
  }

  def newGlobal(options: String = "", extraPhases: Global => List[(SubComponent, String)] = _ => Nil): Global = {
    val settings = new Settings()
    settings.processArgumentString("-usejavacp " + options)
    val reporter = new StoreReporter(settings)
    val g = new Global(settings, reporter) {
      def addToPhasesSet1(comp: SubComponent, desc: String) = addToPhasesSet(comp, desc)
    }
    for ((comp, desc) <- extraPhases(g)) g.addToPhasesSet1(comp, desc)
    new g.Run
    g
  }
  def newInteractiveGlobal(options: String = ""): scala.tools.nsc.interactive.Global = {
    val settings = new Settings()
    settings.processArgumentString("-usejavacp " + options)
    val reporter = new StoreReporter(settings)
    val g = new scala.tools.nsc.interactive.Global(settings, reporter) {
      override def assertCorrectThread(): Unit = ()
    }
    new g.TyperRun
    g
  }
  def newSubcomponent(g: Global, rightAfter: String, desc: String = "test")(f: ((g.type, g.CompilationUnit) => Unit)): (SubComponent, String) = {
    import g._
    val x = new SubComponent {
      override def newPhase(prev: Phase): Phase = new g.GlobalPhase(prev) {
        override def apply(unit: g.CompilationUnit): Unit = f(g, unit)
        override def name: String = phaseName
      }
      override val global: Global = g
      override val runsAfter: List[String] = rightAfter :: Nil
      override val phaseName: String = desc
      override val runsRightAfter: Option[String] = Some(rightAfter)
    }
    (x, desc)
  }

  def compile(code: String, global: Global = newGlobal()): CompileResult[global.type] = {
    val run = new global.Run
    global.reporter.reset()
    val source = global.newSourceFile(code)
    run.compileSources(source :: Nil)
    val unit = run.units.toList.head
    val tree = unit.body
    val infos = global.reporter match {
      case sr: StoreReporter => sr.infos
      case _ => Nil
    }
    new global.Run
    new CompileResult[global.type](global, global.reporter.hasErrors, unit, tree, infos.toList)
  }

  def lookupIdent(g: Global)(name: g.Name): g.Symbol = {
    import g._
    val context = analyzer.newTyper(analyzer.rootContext(NoCompilationUnit)).context
    val lookup = context.lookupSymbol(name, _ => true)
    lookup.symbol
  }

  case class CompileResult[G <: Global](global: G, error: Boolean, unit: G#CompilationUnit, tree: G#Tree, infos: List[StoreReporter.Info]) {
    def assertNoErrors(): this.type = {assert(!error, infos); this}
    def classLoader: ClassLoader = {
      val s = new Settings()
      s.classpath.value = global.settings.classpath.value
      s.classpath.value = s.classpath.value + File.pathSeparator + s.outputDirs.getSingleOutput.get.path
      val classPathURLs = new Global(s).classPath.asURLs
      ScalaClassLoader.fromURLs(classPathURLs, getClass.getClassLoader)
    }
  }

  private var indent = 0
  var debug = true
  def trace[A](msg: String)(body: => A): A = {
    def oneLine(s: String) = s.replaceAll("""\{\n"""", "{ ").replaceAll("\n", "; ")
    def p(msg: String) = if (debug) println((" " * 4 * indent) + oneLine(msg))
    p(msg)
    indent += 1
    val result = try body finally indent -= 1
    p("=> " + result)
    result
  }
}
