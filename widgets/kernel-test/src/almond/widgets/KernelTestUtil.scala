package almond.widgets

import java.io.File
import java.net.{URI, URLClassLoader}
import java.util.UUID
import java.util.jar.{Attributes, JarFile}

import almond.ScalaInterpreterParams
import almond.channels.Channel
import almond.interpreter.Message
import almond.logger.{Level, LoggerContext}
import almond.protocol.{Execute => ProtocolExecute, _}
import ammonite.util.Colors
import cats.effect.IO
import cats.effect.unsafe.IORuntime

import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

/** Helpers to run the Scala kernel in tests
  *
  * Adapted from the ones of the tests of almond (`almond.TestUtil`, `almond.testkit.TestLogging`),
  * that aren't published.
  */
object KernelTestUtil {

  // set ALMOND_TEST_LOGGING to debug, say, to see what the kernel is doing
  lazy val logCtx: LoggerContext = {
    val level = sys.env.get("ALMOND_TEST_LOGGING")
      .orElse(sys.props.get("almond.test.logging"))
      .map { s =>
        Level.fromString(s) match {
          case Left(e)  => sys.error(s"Parsing log level '$s': $e")
          case Right(l) => l
        }
      }
      .getOrElse(Level.Warning)
    LoggerContext.stderr(level)
  }

  implicit class IOOps[T](private val io: IO[T]) extends AnyVal {
    // beware this is not *exactly* a timeout, more a max idle time say… (see the scaladoc of IO.unsafeRunTimed)
    def unsafeRunTimedOrThrow(ioRuntime: IORuntime, duration: Duration = Duration.Inf): T =
      duration match {
        case finite: FiniteDuration =>
          io.unsafeRunTimed(finite)(ioRuntime).getOrElse {
            throw new Exception("Timeout")
          }
        case _ =>
          io.unsafeRunSync()(ioRuntime)
      }
  }

  def execute(
    code: String,
    sessionId: String,
    msgId: String = UUID.randomUUID().toString
  ) =
    Message(
      Header(
        msg_id = msgId,
        username = "test",
        session = sessionId,
        msg_type = ProtocolExecute.requestType.messageType,
        version = Some(Protocol.versionStr)
      ),
      ProtocolExecute.Request(code, stop_on_error = Some(true))
    ).on(Channel.Requests)

  private lazy val initialClassLoader = {

    val contextClassLoader = Thread.currentThread().getContextClassLoader

    val appClassPath = sys.props.get("java.class.path").toSeq.flatMap(_.split(File.pathSeparator))

    // When the class path is passed via a manifest JAR, Ammonite only sees that JAR - add the
    // ones it references in a URLClassLoader, so that it finds them
    val manifestClassPathOpt = appClassPath match {
      case Seq(jar) =>
        val jarFile = new JarFile(jar)
        try
          Option(jarFile.getManifest).flatMap { manifest =>
            Option(manifest.getMainAttributes.get(Attributes.Name.CLASS_PATH)).collect {
              case classPath: String =>
                classPath.split("\\s+").map(new URI(_).toURL)
            }
          }
        finally jarFile.close()
      case _ => None
    }

    manifestClassPathOpt match {
      case Some(manifestClassPath) => new URLClassLoader(manifestClassPath, contextClassLoader)
      case None                    => contextClassLoader
    }
  }

  lazy val interpreterParams = ScalaInterpreterParams(
    initialColors = Colors.BlackWhite,
    initialClassLoader = initialClassLoader
  )
}
