package guide

import java.io.File
import java.net.URL
import java.nio.file.{StandardCopyOption, CopyOption, Files}

object _09_JarLister extends App {
  ===("JAR Lister")
  val jarUrl = "http://central.maven.org/maven2/com/typesafe/akka/akka-actor_2.11/2.3.12/akka-actor_2.11-2.3.12.jar"
  val tempFile = File.createTempFile("akka-", ".jar").toPath
  val is = new URL(jarUrl).openStream()
  try Files.copy(is, tempFile, StandardCopyOption.REPLACE_EXISTING) finally is.close()


  val global = newGlobal(s"-classpath $tempFile")
  import global._
  val akkaPackage = global.rootMirror.getPackage(TermName("akka"))
  def traverse(sym: Symbol): Iterator[Symbol] = {
    try { sym.initialize } catch { case _: Throwable => }
    Iterator(sym) ++ (if (sym.isClass) sym.info.decls.toList.flatMap(traverse) else if (sym.isModule & sym.hasPackageFlag) traverse(sym.moduleClass) else Nil)
  }
  p(traverse(akkaPackage).take(100).map(_.defString).mkString("\n"))
  Files.delete(tempFile)
}
