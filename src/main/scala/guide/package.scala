import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter

package object guide {
  val Width = 80
  def --------------- = println("\n" + ("-" * Width) + "\n")

  def ===(s: String) = p(title('=', s))
  def ---(s: String) = p(title('-', s))

  def title(filler: Char, s: String) = {
    val diff = Width - s.length - 1
    val l = diff / 2
    val r = diff - l
    "\n" + ("=" * l) + " " + s + " " + ("=" * r) + "\n"
  }

  def p(a: Any) = println(a)

  def newGlobal(options: String = ""): Global = {
    val reporter = new StoreReporter
    val settings = new Settings()
    settings.processArgumentString("-usejavacp " + options)
    val g = new Global(settings, reporter)
    new g.Run
    g
  }
  def compile(code: String, global: Global = newGlobal()): CompileResult[global.type] = {
    val run = new global.Run
    global.reporter.reset()
    val source = global.newSourceFile(code)
    run.compileSources(source :: Nil)
    val tree = run.units.toList.head.body
    val infos = global.reporter match {
      case sr: StoreReporter => sr.infos
      case _ => Nil
    }
    new global.Run
    new CompileResult[global.type](global, global.reporter.hasErrors, tree, infos.toList)
  }

  case class CompileResult[G <: Global](global: G, error: Boolean, tree: G#Tree, infos: List[StoreReporter#Info])
}
