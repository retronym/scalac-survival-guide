package guide

import scala.util.matching.Regex


object _19_ScalaSig extends App {
  private val ModuleClassName: Regex = """(.*)\$""".r
  private val ImplClassName: Regex = """(.*)\$class""".r
  def isScala(cls: Class[_]) = {
    import scala.reflect.{ScalaSignature, ScalaLongSignature}
    def hasAnn(cls: Class[_]): Boolean = {
      val anns = List(classOf[ScalaSignature], classOf[ScalaLongSignature])
      anns.exists(ann => cls.getDeclaredAnnotation(ann) != null)
    }
    def classForName(name: String, init: Boolean, loader: ClassLoader): Option[Class[_]] = try {
      Some(Class.forName(name, init, loader))
    } catch {
      case _: ClassNotFoundException =>
        None
    }

    def topLevelClass(cls: Class[_]): Class[_] = {
      if (cls.getEnclosingClass != null) topLevelClass(cls.getEnclosingClass)
      else {
        cls.getName match {
          case ModuleClassName(companionClassName) =>
            classForName(companionClassName, init = false, cls.getClassLoader).getOrElse(cls)
          case ImplClassName(interfaceName) =>
            classForName(interfaceName, init = false, cls.getClassLoader).getOrElse(cls)
          case _ => cls
        }
      }
    }
    hasAnn(topLevelClass(cls))
  }

  assert(!isScala(classOf[String]))
  def check(o: AnyRef) = assert(isScala(o.getClass))
  val c = new C
  check(c)
  check(new c.C_Inner1)
  check(c.localAnon)
  check(c.localNamed)
  check(c.lambda)
  check(C)
  check(new C().localAnon)
  check(new C().localNamed)
  check(new C.C_Inner2())
  check(C.localAnon)
  check(C.localNamed)
  check(C.lambda)
  check(O)
  check(new O.O_Inner())
  check(O.localAnon)
  check(O.localNamed)
  check(O.lambda)

  assert(isScala(new T {}.traitImplClass))

}

class C {
  class C_Inner1
  def localAnon = new Object {}
  def localNamed = {object Local; Local}
  def lambda = {() => 42}
}
object C {
  class C_Inner2
  def localAnon = new Object {}
  def localNamed = {object Local; Local}
  def lambda = {() => 42}
}
object O {
  class O_Inner
  def localAnon = new Object {}
  def localNamed = {object Local; Local}
  def lambda = {() => 42}
}
trait T {
  def traitImplClass: Class[_] = sun.reflect.Reflection.getCallerClass(1)
}