package almond.widgets

import almond.channels.{Channel, Message => RawMessage}
import almond.interpreter.Message
import almond.interpreter.messagehandlers.MessageHandler
import almond.protocol.Codecs.stringCodec
import almond.protocol.{Execute, MessageType, RawJson}
import cats.effect.IO
import cats.effect.std.Queue
import cats.effect.unsafe.IORuntime
import com.github.plokhotnyuk.jsoniter_scala.core._
import fs2.{Pipe, Stream}

import scala.collection.mutable
import scala.util.Try

/** Streams to run a kernel with, acting as its client
  *
  * Trimmed copy of `almond.testkit.ClientStreams`, from the tests of almond, that aren't published.
  */
final case class ClientStreams(
  source: Stream[IO, (Channel, RawMessage)],
  sink: Pipe[IO, (Channel, RawMessage), Unit],
  // Left: from the kernel, Right: to the kernel
  generatedMessages: mutable.ListBuffer[Either[
    (Channel, Message[RawJson]),
    (Channel, Message[RawJson])
  ]]
) {

  import ClientStreams.RawJsonOps

  // from kernel
  def singleReply[T: JsonValueCodec](channel: Channel, msgType: MessageType[T]): Message[T] = {

    val l = generatedMessages
      .collect {
        case Left((`channel`, m)) if m.header.msg_type == msgType.messageType =>
          m.decodeAs[T] match {
            case Left(err) =>
              throw new Exception(s"Error decoding message: $err${System.lineSeparator()}$m")
            case Right(m0) =>
              m0
          }
      }
      .toList

    l match {
      case Nil     => throw new Exception(s"No message of type $msgType on $channel")
      case List(m) => m
      case _ => throw new Exception(s"Too many messages of type $msgType on $channel (${l.length})")
    }
  }

  def executeResults: Map[Int, String] =
    generatedMessages
      .iterator
      .collect {
        case Left((Channel.Publish, m)) if m.header.msg_type == Execute.resultType.messageType =>
          m.decodeAs[Execute.Result] match {
            case Left(_)  => Nil
            case Right(m) => Seq(m.content)
          }
      }
      .flatten
      .map { r =>
        r.execution_count -> r.data.get("text/plain").fold("")(_.stringOrEmpty)
      }
      .toMap

  def executeErrors: Map[Int, (String, String, List[String])] =
    generatedMessages
      .iterator
      .collect {
        case Left((Channel.Requests, m)) if m.header.msg_type == Execute.replyType.messageType =>
          m.decodeAs[Execute.Reply] match {
            case Left(_)  => Nil
            case Right(m) => Seq(m.content)
          }
      }
      .flatten
      .collect {
        case e: Execute.Reply.Error =>
          (e.execution_count, (e.ename, e.evalue, e.traceback))
      }
      .toMap

  /** What was printed on stdout */
  def output: Seq[String] =
    generatedMessages
      .iterator
      .collect {
        case Left((Channel.Publish, m)) if m.header.msg_type == "stream" =>
          m.decodeAs[Execute.Stream] match {
            case Left(_) => Nil
            case Right(m) =>
              if (m.content.name == "stdout")
                Seq(m.content.text)
              else
                Nil
          }
      }
      .flatten
      .toList
}

object ClientStreams {

  implicit class RawJsonOps(private val rawJson: RawJson) extends AnyVal {
    def stringOrEmpty: String =
      Try(readFromArray[String](rawJson.value)).toOption.getOrElse("")
  }

  def create(
    initialMessages: Stream[IO, (Channel, RawMessage)],
    stopWhen: (Channel, Message[RawJson]) => IO[Boolean],
    handler: MessageHandler,
    ioRuntime: IORuntime
  ): ClientStreams = {

    val b = new mutable.ListBuffer[Either[(Channel, Message[RawJson]), (Channel, Message[RawJson])]]

    val poisonPill: (Channel, RawMessage) = null

    val q = Queue.unbounded[IO, (Channel, RawMessage)].unsafeRunSync()(ioRuntime)

    val sink: Pipe[IO, (Channel, RawMessage), Unit] = { s =>

      val s0 = Stream.bracket(IO.unit)(_ => q.offer(poisonPill))
        .flatMap(_ => s)

      s0.evalMap {
        case (c, m) =>
          Message.parse[RawJson](m) match {
            case Left(e) =>
              IO.raiseError(new Exception(s"Error decoding message: $e"))
            case Right(m0) =>
              b += Left(c -> m0)

              val extra = stopWhen(c, m0).flatMap {
                case true =>
                  q.offer(poisonPill)
                case false =>
                  IO.unit
              }

              val resp = handler.handle(c, m0) match {
                case None =>
                  IO.raiseError(
                    new Exception(s"Unhandled message on $c of type ${m0.header.msg_type}: $m")
                  )
                case Some(Left(e)) =>
                  IO.raiseError(new Exception("Error processing message", e))
                case Some(Right(s)) =>
                  s.evalMap(q.offer).compile.drain
              }

              for {
                a <- resp.attempt
                _ <- extra
                r <- IO.fromEither(a)
              } yield r
          }
      }
    }

    ClientStreams(
      initialMessages ++ Stream.repeatEval(q.take).takeWhile(_ != poisonPill).evalMap {
        case (c, m) =>
          Message.parse[RawJson](m) match {
            case Left(e) =>
              IO.raiseError(new Exception(s"Error decoding message: $e"))
            case Right(m0) =>
              IO {
                b += Right(c -> m0)
                (c, m)
              }
          }
      },
      sink,
      b
    )
  }

}
