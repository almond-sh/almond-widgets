package widgetsbuild

import coursier.version.Version

/** The Scala versions we build and test with, following the ones of almond (see its ScalaVersions)
  */
object ScalaVersions {
  def scala3Latest   = "3.9.0"
  def scala3Compat   = "3.3.8"  // scala-steward:off
  def scala213       = "2.13.18"
  def scala212       = "2.12.21"
  def scala213Oldest = "2.13.3" // scala-steward:off
  def scala212Oldest = "2.12.8" // scala-steward:off

  // Like almond, we publish one module per binary Scala version, built with the oldest full Scala
  // version almond supports for it: Scala 2 patch releases are only forward binary compatible, so
  // building widgets_2.13 with 2.13.3 gives an artifact all the 2.13.x kernels can load. For Scala
  // 3, the LTS plays that role.
  val binaries = Seq(scala3Compat, scala213Oldest, scala212Oldest)

  // The tests are built and run with full Scala versions: the latest ones, along with 2.13.3 and
  // 3.3.8, that the modules we publish are built with. Not 2.12.8, whose compiler can't expand the
  // macros of utest on the JVMs Mill runs it on.
  val tests = Seq(scala3Latest, scala3Compat, scala213, scala213Oldest, scala212)

  /** The [[binaries]] entry a full Scala version's modules are built under. */
  def binary(sv: String) =
    if (sv.startsWith("2.12.")) scala212Oldest
    else if (sv.startsWith("2.13.")) scala213Oldest
    else scala3Compat

  /** The [[tests]] entries testing the module of a [[binaries]] entry */
  def testsFor(binaryScalaVersion: String): Seq[String] =
    tests.filter(binary(_) == binaryScalaVersion)

  /** The JVM to run the compiler of a full Scala version on, when the one Mill runs on is too
    * recent for it: the Scala 2.12.x before 2.12.18 and the 2.13.x before 2.13.11 can't read the
    * class files of JDK 21. JDK 17 is the oldest JVM Mill runs scalac on.
    */
  def jvmVersionFor(sv: String): Option[String] = {
    val needsJvm17 =
      (sv.startsWith("2.12.") && Version(sv) < Version("2.12.18")) ||
      (sv.startsWith("2.13.") && Version(sv) < Version("2.13.11"))
    if (needsJvm17) Some("17")
    else None
  }
}
