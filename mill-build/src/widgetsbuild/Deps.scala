package widgetsbuild

import mill.scalalib.*

object Deps {
  def almondApi              = mvn"sh.almond::api:${Versions.almond}"
  def almondKernel           = mvn"sh.almond::kernel:${Versions.almond}"
  def almondScalaInterpreter = mvn"sh.almond::scala-interpreter:${Versions.almond}"
  // the kernel pulls the compiler of the oldest Scala version of its binary version, depending on
  // it explicitly gets the one of the Scala version the tests run with
  def scalaCompiler(sv: String) =
    if (sv.startsWith("2.")) mvn"org.scala-lang:scala-compiler:$sv"
    else mvn"org.scala-lang::scala3-compiler:$sv"
  // same version as the one the Ammonite fork of almond depends on
  def ujson = mvn"com.lihaoyi::ujson:4.3.2"
  def utest = mvn"com.lihaoyi::utest:0.9.5"
}
