package almond.widgets

import java.nio.charset.StandardCharsets
import java.util.UUID

import almond.ScalaInterpreter
import almond.channels.Channel
import almond.interpreter.Message
import almond.interpreter.messagehandlers.MessageHandler
import almond.kernel.{Kernel, KernelThreads}
import almond.protocol.{Execute => ProtocolExecute, _}
import almond.widgets.KernelTestUtil.{IOOps, execute => executeMessage, interpreterParams, logCtx}
import almond.util.ThreadUtil.{
  attemptShutdownExecutionContext,
  singleThreadedExecutionContextExecutorService
}
import cats.effect.IO
import fs2.Stream
import utest._

/** Tests of widgets running in the Scala kernel, along with a pseudo front-end */
object WidgetsKernelTests extends TestSuite {

  val interpreterEc  = singleThreadedExecutionContextExecutorService("test-interpreter")
  val cancellablesEc = singleThreadedExecutionContextExecutorService("test-cancellables")

  val threads = KernelThreads.create("test")

  def createKernel(): Kernel = {
    val interpreter = new ScalaInterpreter(
      params = interpreterParams,
      logCtx = logCtx
    )
    Kernel.create(interpreter, interpreterEc, threads, cancellablesEc, logCtx)
      .unsafeRunTimedOrThrow(threads.ioRuntime)
  }

  /** Stops the kernel once it replied to the execute request with ID `msgId` */
  def stopAfter(msgId: String): (Channel, Message[RawJson]) => IO[Boolean] =
    (_, m) =>
      IO.pure(
        m.header.msg_type == "execute_reply" &&
        m.parent_header.exists(_.msg_id == msgId)
      )

  val ignored = MessageHandler.discard {
    case (Channel.Publish, _)                                          =>
    case (Channel.Requests, m) if m.header.msg_type == "execute_reply" =>
  }

  override def utestAfterAll() = {
    threads.attemptShutdown()
    if (!attemptShutdownExecutionContext(interpreterEc))
      println(s"Don't know how to shutdown $interpreterEc")
  }

  val tests = Tests {

    test("widgets") {

      val kernel    = createKernel()
      val sessionId = UUID.randomUUID().toString

      def request[T: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec](
        msgType: MessageType[T],
        content: T,
        buffers: Seq[Array[Byte]] = Nil
      ) =
        Message(Header.random("test", msgType, sessionId), content, buffers = buffers)
          .on(Channel.Requests)

      def json(raw: RawJson) =
        ujson.read(raw.value) match {
          case obj: ujson.Obj => obj
          case other          => sys.error(s"Unexpected JSON: $other")
        }

      val lastMsgId = UUID.randomUUID().toString
      val updates   = new scala.collection.mutable.ListBuffer[String]

      // How the pseudo front-end behaves: when the kernel opens the slider and the upload widgets,
      // it changes their values, then asks for the open comms. Once these are known, it checks the
      // kernel got the new values.
      val frontend = MessageHandler.blocking(
        Channel.Publish,
        Comm.openType,
        almond.interpreter.KernelSession(UUID.randomUUID().toString, "test"),
        threads.queueEc,
        logCtx,
        publishStatus = false
      ) { (msg, queue) =>
        val state = json(msg.content.data).value.get("state") match {
          case Some(obj: ujson.Obj) => obj
          case _                    => sys.error("No state in comm_open")
        }
        val id = msg.content.comm_id
        def update(state: (String, ujson.Value)*)(paths: String*) =
          RawJson(
            ujson.Obj(
              "method" -> ujson.Str("update"),
              "state"  -> ujson.Obj.from(state),
              "buffer_paths" -> ujson.Arr.from(
                paths.map { p =>
                  ujson.Arr.from(
                    p.split('.').toSeq.map { elem =>
                      if (elem.forall(_.isDigit)) ujson.Num(elem.toInt)
                      else ujson.Str(elem)
                    }
                  )
                }
              )
            ).render().getBytes(StandardCharsets.UTF_8)
          )
        val responses = state.value.get("_model_name") match {
          case Some(ujson.Str("IntSliderModel")) =>
            updates += id
            Seq(request(
              Comm.messageType,
              Comm.Message(id, update("value" -> ujson.Num(7))())
            ))
          case Some(ujson.Str("FileUploadModel")) =>
            val file = ujson.Obj(
              "name"          -> ujson.Str("hello.txt"),
              "type"          -> ujson.Str("text/plain"),
              "size"          -> ujson.Num(5),
              "last_modified" -> ujson.Num(0)
            )
            updates += id
            Seq(
              request(
                Comm.messageType,
                Comm.Message(
                  id,
                  update("value" -> ujson.Arr(file))("value.0.content")
                ),
                buffers = Seq("hello".getBytes(StandardCharsets.UTF_8))
              )
            )
          case Some(ujson.Str("HBoxModel")) =>
            Seq(request(CommInfo.requestType, CommInfo.Request(Some("jupyter.widget"))))
          case _ =>
            Nil
        }
        responses.foldLeft(IO.unit)((acc, m) => acc.flatMap(_ => queue.offer(m)))
      }

      val checkValues = MessageHandler(Channel.Requests, CommInfo.replyType) { _ =>
        Stream(
          executeMessage(
            """println(s.value())
              |println(new String(u.value().head.content, "UTF-8"))""".stripMargin,
            sessionId,
            msgId = lastMsgId
          )
        )
      }

      val input = Stream(
        executeMessage(
          """import almond.widgets._
            |val s = IntSlider(value = 3)
            |val u = FileUpload()
            |s.value.observe(_ => ())
            |HBox(Seq(s, u))""".stripMargin,
          sessionId
        )
      )

      val streams = ClientStreams.create(
        input,
        stopAfter(lastMsgId),
        frontend.orElse(checkValues, ignored),
        ioRuntime = threads.ioRuntime
      )

      kernel.run(streams.source, streams.sink, Nil)
        .unsafeRunTimedOrThrow(threads.ioRuntime)

      val fromKernel = streams.generatedMessages.toList.collect {
        case Left((Channel.Publish, m)) => m
      }

      // widgets are opened before being displayed
      val openIdx = fromKernel.zipWithIndex.collect {
        case (m, idx) if m.header.msg_type == "comm_open" =>
          m.decodeAs[Comm.Open].toOption.get.content.comm_id -> idx
      }.toMap
      // the slider, the upload widget, the box, their 3 layouts, and the styles of the first 2
      assert(openIdx.size == 8)
      val displayed = fromKernel.zipWithIndex.collect {
        case (m, idx) if m.header.msg_type == "display_data" =>
          val data  = m.decodeAs[ProtocolExecute.DisplayData].toOption.get.content.data
          val view  = json(data(almond.widgets.Widget.viewMimeType))
          val model = view.value.get("model_id").collect { case ujson.Str(id) => id }
          model.get -> idx
      }
      // only the box is displayed, like in ipywidgets, widgets assigned to vals aren't
      assert(displayed.length == 1)
      for ((id, idx) <- displayed)
        assert(openIdx.get(id).exists(_ < idx))
      val firstResult = streams.executeResults.getOrElse(1, "")
      assert(firstResult.contains("s: IntSlider = IntSlider(value=3)"))
      assert(firstResult.contains("u: FileUpload = FileUpload()"))
      // the handle returned by observe isn't printed
      assert(!firstResult.contains("Subscription"))

      // changes from the front-end are echoed back, in response to the message they came in
      val echoes = fromKernel.filter { m =>
        m.header.msg_type == "comm_msg" &&
        m.decodeAs[Comm.Message].toOption.exists { m0 =>
          json(m0.content.data).value.get("method").contains(ujson.Str("echo_update"))
        }
      }
      // the uploaded file isn't echoed
      assert(echoes.length == 1)
      val sliderUpdate = streams.generatedMessages.toList.collect {
        case Right((Channel.Requests, m)) if m.header.msg_type == "comm_msg" => m
      }.head
      assert(echoes.head.parent_header.map(_.msg_id) == Some(sliderUpdate.header.msg_id))

      val commInfo = streams.singleReply(Channel.Requests, CommInfo.replyType)
      assert(updates.forall(commInfo.content.comms.contains))
      assert(commInfo.content.comms.values.forall(_.target_name == "jupyter.widget"))
      assert(commInfo.content.comms.size == 8)

      val ls = System.lineSeparator()
      assert(streams.output.mkString.endsWith(s"7${ls}hello$ls"))
    }

    test("widgets created by code defined in cells") {
      val kernel    = createKernel()
      val sessionId = UUID.randomUUID().toString
      val lastMsgId = UUID.randomUUID().toString

      val input = Stream(
        // the implicit widget manager of cells gets passed to the things they define
        executeMessage(
          """def labelled(text: String): almond.widgets.HBox =
            |  almond.widgets.HBox(Seq(almond.widgets.Label(value = text), almond.widgets.IntSlider()))
            |class Counter extends almond.widgets.AnyWidget("export default { render() {} }") {
            |  val count = prop("count", 0)
            |}""".stripMargin,
          sessionId
        ),
        executeMessage(
          """println {
            |  val box     = labelled("n")
            |  val counter = new Counter
            |  counter.count() = 2
            |  Seq(box, counter).forall(_.manager eq almond.widgets.IntSlider().manager)
            |}""".stripMargin,
          sessionId,
          msgId = lastMsgId
        )
      )

      val streams = ClientStreams.create(
        input,
        stopAfter(lastMsgId),
        ignored,
        ioRuntime = threads.ioRuntime
      )

      kernel.run(streams.source, streams.sink, Nil)
        .unsafeRunTimedOrThrow(threads.ioRuntime)

      assert(streams.executeErrors.isEmpty)
      val firstResult = streams.executeResults.getOrElse(1, "").replace("\r\n", "\n")
      assert(firstResult == "defined function labelled\ndefined class Counter")
      assert(streams.output.mkString == "true" + System.lineSeparator())
    }
  }
}
