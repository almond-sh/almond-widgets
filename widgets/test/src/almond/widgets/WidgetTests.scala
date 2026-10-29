package almond.widgets

import almond.display.Html
import almond.interpreter.api.CommHandler
import almond.widgets.TestCommHandler.Sent
import utest._

import java.nio.charset.StandardCharsets
import java.time.{Instant, LocalDate, LocalTime}

import scala.collection.mutable

// like the example of the documentation
class MyWidget(implicit manager: WidgetManager) extends DOMWidget(manager) {
  def modelName          = "MyModel"
  def modelModule        = "my-widgets"
  def modelModuleVersion = "^1.0.0"
  def viewName           = Some("MyView")
  def viewModule         = "my-widgets"
  def viewModuleVersion  = "^1.0.0"
  val layout             = prop("layout", Layout())
  val count              = prop("count", 0)
}

object WidgetTests extends TestSuite {

  private final class Env {
    val comm    = new TestCommHandler
    val manager = new WidgetManager(comm, echo = true)
    manager.registerTargets()
  }

  private def withEnv[T](f: Env => T): T =
    f(new Env)

  // the manager widgets get created with in tests
  private implicit def envManager(implicit env: Env): WidgetManager =
    env.manager

  private def str(s: String): ujson.Value = ujson.Str(s)
  private def num(d: Double): ujson.Value = ujson.Num(d)
  private def ref(w: Widget): ujson.Value = ujson.Str("IPY_MODEL_" + w.id)
  private def update(fields: (String, ujson.Value)*): ujson.Value =
    ujson.Obj(
      "method"       -> str("update"),
      "state"        -> ujson.Obj.from(fields),
      "buffer_paths" -> ujson.Arr()
    )

  val tests = Tests {

    test("encoded") {
      val a = Array[Byte](1, 2, 3)
      val b = Array[Byte](4, 5)
      val value = Encoded.obj(
        "x" -> Encoded.bytes(a),
        "y" -> Encoded.arr(Seq(
          Encoded(num(1)),
          Encoded.obj("z" -> Encoded.bytes(b), "t" -> Encoded(str("t"))),
          Encoded.bytes(a)
        ))
      )
      assert(value.field("y").element(1).field("z").bytes.map(_.toSeq) == Some(b.toSeq))
      assert(value.fieldNames == Seq("x", "y"))

      // binary values are removed from objects, and are null in arrays
      val (json, paths, buffers) = Encoded.toMessage(value)
      assert(json == ujson.Obj("y" -> ujson.Arr(num(1), ujson.Obj("t" -> str("t")), ujson.Null)))
      assert(
        paths == ujson.Arr(
          ujson.Arr(str("x")),
          ujson.Arr(str("y"), num(1), str("z")),
          ujson.Arr(str("y"), num(2))
        )
      )
      assert(buffers.map(_.toSeq) == Seq(a.toSeq, b.toSeq, a.toSeq))
      // the value itself isn't changed
      assert(value.json("y")(1).obj.contains("z"))

      val restored = Encoded.fromMessage(json, paths.value.toSeq, buffers)
      assert(restored.field("x") == value.field("x"))
      assert(restored.field("y") == value.field("y"))
    }

    test("lazy open") {
      withEnv { implicit env =>
        val slider = IntSlider(value = 3, min = 1, max = 5, description = "n")
        slider.value() = 4
        assert(env.comm.take().isEmpty)

        val data   = slider.displayData()
        val opened = env.comm.take()
        // layout and style first, then the slider referencing them
        assert(opened.map(_.kind) == List("open", "open", "open"))
        assert(opened.last.id == slider.id)
        assert(opened.map(_.id).take(2).toSet == Set(slider.layout().id, slider.style().id))
        val state = opened.last.state
        assert(state.value.get("_model_name") == Some(str("IntSliderModel")))
        assert(state.value.get("_view_name") == Some(str("IntSliderView")))
        assert(state.value.get("_model_module") == Some(str("@jupyter-widgets/controls")))
        assert(state.value.get("value") == Some(num(4)))
        assert(state.value.get("min") == Some(num(1)))
        assert(state.value.get("description") == Some(str("n")))
        assert(state.value.get("layout") == Some(ref(slider.layout())))
        assert(state.value.get("tooltip") == Some(ujson.Null))
        assert(opened.last.metadata == ujson.Obj("version" -> str("2.1.0")))

        val viewData = ujson.read(data.detailedData(Widget.viewMimeType) match {
          case almond.interpreter.api.DisplayData.Value.Json(s) => s
          case other                                            => sys.error(s"Unexpected $other")
        })
        assert(
          viewData == ujson.Obj(
            "version_major" -> num(2),
            "version_minor" -> num(0),
            "model_id"      -> str(slider.id)
          )
        )

        // already open
        slider.displayData()
        assert(env.comm.take().isEmpty)
      }
    }

    test("toString") {
      withEnv { implicit env =>
        // fields that differ from their defaults, in the order properties are declared, whatever
        // the order they were set in (ListMap#updated moves keys last in Scala 2.12)
        val slider = IntSlider(value = 5, description = "n", max = 10)
        assert(slider.toString == "IntSlider(description=\"n\", max=10, value=5)")
        slider.value() = 6
        assert(slider.toString == "IntSlider(description=\"n\", max=10, value=6)")
        assert(Label().toString == "Label()")
      }
    }

    test("kernel-side updates") {
      withEnv { implicit env =>
        val text = Text(value = "a").open()
        env.comm.take()
        text.value() = "b"
        text.value() = "b" // no change, nothing sent
        val sent = env.comm.take()
        assert(sent.length == 1)
        assert(sent.head.method == Some("update"))
        assert(sent.head.state == ujson.Obj("value" -> str("b")))

        text.holdSync {
          text.value() = "c"
          text.placeholder() = "type here"
          text.value() = "d"
        }
        val held = env.comm.take()
        assert(held.length == 1)
        assert(
          held.head.state == ujson.Obj("value" -> str("d"), "placeholder" -> str("type here"))
        )
      }
    }

    test("front-end updates") {
      withEnv { implicit env =>
        val slider = FloatSlider(value = 1.5).open()
        env.comm.take()
        val seen = new mutable.ListBuffer[(Double, Double)]
        slider.value.observeChange((former, value) => seen += former -> value)

        env.comm.frontendMessage(slider.id, update("value" -> num(2.5)))
        assert(slider.value() == 2.5)
        assert(seen.toList == List(1.5 -> 2.5))
        val echo = env.comm.take()
        assert(echo.map(_.method) == List(Some("echo_update")))
        assert(echo.head.state == ujson.Obj("value" -> num(2.5)))

        // observers changing other properties
        slider.value.observe(v => slider.description() = s"at $v")
        env.comm.frontendMessage(slider.id, update("value" -> num(3)))
        assert(slider.description() == "at 3.0")
        val sent = env.comm.take()
        assert(sent.map(_.method) == List(Some("echo_update"), Some("update")))
        assert(sent(1).state == ujson.Obj("description" -> str("at 3.0")))

        env.comm.frontendMessage(slider.id, ujson.Obj("method" -> str("request_state")))
        val full = env.comm.take()
        assert(full.map(_.method) == List(Some("update")))
        assert(full.head.state.value.get("value") == Some(num(3)))
        assert(full.head.state.value.get("_model_name") == Some(str("FloatSliderModel")))
      }
    }

    test("manager from comm handler") {
      val comm                              = new TestCommHandler
      implicit val commHandler: CommHandler = comm
      val slider                            = IntSlider()
      // a single manager per comm handler, that registered its comm targets
      assert(slider.manager eq Label().manager)
      assert(slider.manager eq WidgetManager.fromCommHandler)
      assert(comm.targets.contains(WidgetManager.targetName))
      assert(comm.targets.contains(WidgetManager.controlTargetName))
      val other = WidgetManager.fromCommHandler(new TestCommHandler)
      assert(other ne slider.manager)
    }

    test("no echo") {
      val comm                            = new TestCommHandler
      implicit val manager: WidgetManager = new WidgetManager(comm, echo = false)
      manager.registerTargets()
      val box = Checkbox().open()
      comm.take()
      comm.frontendMessage(box.id, update("value" -> ujson.Bool(true)))
      assert(box.value())
      assert(comm.take().isEmpty)
    }

    test("button") {
      withEnv { implicit env =>
        val button = Button(description = "Click me").open()
        var clicks = 0
        val sub    = button.onClick(clicks += 1)
        val click = ujson.Obj(
          "method"  -> str("custom"),
          "content" -> ujson.Obj("event" -> str("click"))
        )
        env.comm.frontendMessage(button.id, click)
        env.comm.frontendMessage(button.id, click)
        assert(clicks == 2)
        sub.cancel()
        env.comm.frontendMessage(button.id, click)
        assert(clicks == 2)
      }
    }

    test("exceptions in callbacks") {
      withEnv { implicit env =>
        val button = Button().open()
        var clicks = 0
        button.onClick(throw new Exception("foo"))
        button.onClick(clicks += 1)
        val err      = new java.io.ByteArrayOutputStream
        val ps       = new java.io.PrintStream(err, true, "UTF-8")
        val previous = System.err
        try {
          System.setErr(ps)
          button.click()
        }
        finally System.setErr(previous)
        assert(clicks == 1)
        assert(err.toString("UTF-8").contains("java.lang.Exception: foo"))
      }
    }

    test("selection") {
      withEnv { implicit env =>
        val dropdown = Dropdown(options = Seq(1, 2, 3), labels = Seq("one", "two", "three"))
        assert(dropdown.value() == Some(1))
        assert(dropdown.label() == Some("one"))
        assert(dropdown.index() == Some(0))
        dropdown.value() = Some(3)
        assert(dropdown.index() == Some(2))
        dropdown.open()
        val opened = env.comm.take().last
        assert(opened.state.value.get("index") == Some(num(2)))
        assert(
          opened.state.value.get("_options_labels") == Some(
            ujson.Arr(str("one"), str("two"), str("three"))
          )
        )

        val values = new mutable.ListBuffer[Option[Int]]
        dropdown.value.observe(values += _)
        env.comm.frontendMessage(dropdown.id, update("index" -> num(1)))
        assert(dropdown.value() == Some(2))
        assert(values.toList == List(Some(2)))

        val failure = scala.util.Try(dropdown.value() = Some(4))
        assert(failure.isFailure)

        env.comm.take()
        dropdown.setOptions(Seq(10, 20))
        assert(dropdown.labels == Seq("10", "20"))
        assert(dropdown.value() == Some(10))
        val sent = env.comm.take()
        assert(sent.length == 1)
        assert(sent.head.state.value.get("_options_labels") == Some(ujson.Arr(
          str("10"),
          str("20")
        )))
        assert(sent.head.state.value.get("index") == Some(num(0)))

        val none = Dropdown[String]()
        assert(none.value() == None)

        val multiple = SelectMultiple(options = Seq("a", "b", "c"), value = Seq("a", "c"))
        assert(multiple.index() == Seq(0, 2))
        assert(multiple.label() == Seq("a", "c"))

        val range =
          SelectionRangeSlider(options = Seq('a', 'b', 'c'), value = Some(('b', 'c')))
        assert(range.index() == (1, 2))

        val slider = SelectionSlider(options = Seq("x", "y"))
        assert(slider.value() == "x")
      }
    }

    test("containers") {
      withEnv { implicit env =>
        val a   = Label(value = "a")
        val b   = HTML(value = "<b>b</b>")
        val box = VBox(Seq(a, b))
        box.open()
        val opened = env.comm.take()
        assert(opened.last.id == box.id)
        val openedIds = opened.map(_.id).toSet
        assert(openedIds.contains(a.id))
        assert(openedIds.contains(b.id))
        assert(opened.last.state.value.get("children") == Some(ujson.Arr(ref(a), ref(b))))
        assert(box.children() == Seq(a, b))

        val formerChildren = new mutable.ListBuffer[Seq[Widget]]
        box.children.observeChange((former, _) => formerChildren += former)

        val c = Button()
        box.children() = box.children() :+ c
        val sent = env.comm.take()
        // the new child (and its layout and style) gets opened before being referenced
        assert(sent.map(_.kind) == List("open", "open", "open", "msg"))
        assert(sent(2).id == c.id)
        assert(sent.last.state.value.get("children") == Some(ujson.Arr(ref(a), ref(b), ref(c))))
        assert(c.isOpen)
        assert(formerChildren.toList == List(Seq(a, b)))

        // references to widgets that aren't open
        val d         = Label()
        val e         = Label()
        val unopened  = HBox(Seq(d))
        val formerOne = new mutable.ListBuffer[Seq[Widget]]
        unopened.children.observeChange((former, _) => formerOne += former)
        unopened.children() = Seq(e)
        assert(unopened.children() == Seq(e))
        assert(formerOne.toList == List(Seq(d)))
        assert(!d.isOpen && !e.isOpen)
      }
    }

    test("binary values") {
      withEnv { implicit env =>
        val bytes  = Array[Byte](1, 2, 3, 4)
        val image  = Image(value = bytes, format = "png").open()
        val opened = env.comm.take().last
        assert(opened.id == image.id)
        assert(!opened.state.value.contains("value"))
        assert(opened.bufferPaths == ujson.Arr(ujson.Arr(str("value"))))
        assert(opened.buffers.map(_.toSeq) == Seq(bytes.toSeq))

        image.value() = Array[Byte](5)
        val sent = env.comm.take()
        assert(sent.length == 1)
        assert(sent.head.bufferPaths == ujson.Arr(ujson.Arr(str("value"))))
        assert(sent.head.buffers.map(_.toSeq) == Seq(Seq[Byte](5)))

        val url = Image.fromUrl("https://example.com/a.png")
        assert(url.format() == "url")
        assert(new String(url.value(), StandardCharsets.UTF_8) == "https://example.com/a.png")
      }
    }

    test("file upload") {
      withEnv { implicit env =>
        val upload = FileUpload(multiple = true).open()
        env.comm.take()
        val files = new mutable.ListBuffer[Seq[UploadedFile]]
        upload.value.observe(files += _)
        val content = "hello".getBytes(StandardCharsets.UTF_8)
        val msg = ujson.Obj(
          "method" -> str("update"),
          "state" -> ujson.Obj(
            "value" -> ujson.Arr(
              ujson.Obj(
                "name"          -> str("hello.txt"),
                "type"          -> str("text/plain"),
                "size"          -> num(5),
                "last_modified" -> num(1700000000000.0)
              )
            )
          ),
          "buffer_paths" -> ujson.Arr(ujson.Arr(str("value"), num(0), str("content")))
        )
        env.comm.frontendMessage(upload.id, msg, Seq(content))
        val value = upload.value()
        assert(value.length == 1)
        assert(value.head.name == "hello.txt")
        assert(value.head.mimeType == "text/plain")
        assert(value.head.size == 5L)
        assert(new String(value.head.content, StandardCharsets.UTF_8) == "hello")
        assert(value.head.lastModified == Instant.ofEpochMilli(1700000000000L))
        assert(files.length == 1)
        // file contents aren't echoed
        assert(env.comm.take().isEmpty)
      }
    }

    test("output") {
      withEnv { implicit env =>
        val out = Output().open()
        env.comm.take()
        out.capture {
          println("a")
          print("b")
          // "\r\n" gets normalized to "\n", even on Windows, but lone "\r" are kept
          print("\r")
          Console.err.println("c")
        }
        val outputs = out.outputs()
        assert(
          outputs == Seq(
            ujson.Obj(
              "output_type" -> str("stream"),
              "name"        -> str("stdout"),
              "text"        -> str("a\nb\r")
            ),
            ujson.Obj(
              "output_type" -> str("stream"),
              "name"        -> str("stderr"),
              "text"        -> str("c\n")
            )
          )
        )
        assert(env.comm.take().forall(_.method == Some("update")))

        out.clearOutput()
        out.capture {
          Html("<b>hi</b>").display()(out.outputHandler)
          throw new Exception("boom")
        }
        val outputs0 = out.outputs()
        assert(outputs0.length == 2)
        val display = outputs0.head
        assert(display("output_type") == str("display_data"))
        assert(display("data") == ujson.Obj("text/html" -> str("<b>hi</b>")))
        val error = outputs0(1)
        assert(error("output_type") == str("error"))
        assert(error("evalue") == str("boom"))

        // outputs() gives copies of the outputs
        outputs0.head("output_type") = str("changed")
        assert(out.outputs().head("output_type") == str("display_data"))
      }
    }

    test("links") {
      withEnv { implicit env =>
        val a        = IntSlider()
        val b        = IntText()
        val jsLink   = jslink(a.value, b.value)
        val sent     = env.comm.take()
        val linkOpen = sent.find(_.id == jsLink.id).get
        assert(linkOpen.state.value.get("source") == Some(ujson.Arr(ref(a), str("value"))))
        assert(linkOpen.state.value.get("target") == Some(ujson.Arr(ref(b), str("value"))))
        assert(a.isOpen && b.isOpen)

        val c   = IntSlider(value = 2)
        val d   = IntSlider()
        val sub = link(c.value, d.value)
        assert(d.value() == 2)
        d.value() = 5
        assert(c.value() == 5)
        sub.cancel()
        d.value() = 6
        assert(c.value() == 5)

        val e = FloatText()
        dlink(c.value, e.value, (n: Int) => n * 1.5)
        assert(e.value() == 7.5)
        c.value() = 2
        assert(e.value() == 3.0)
      }
    }

    test("interact") {
      withEnv { implicit env =>
        val slider = IntSlider(value = 3)
        val box    = interact(slider)(n => n * 2)
        val out    = box.children().last.asInstanceOf[Output]
        def text() =
          out.outputs().map(_.obj.get("text"))
        assert(text() == Seq(Some(str("6\n"))))
        slider.value() = 4
        assert(text() == Seq(Some(str("8\n"))))
      }
    }

    test("dates") {
      withEnv { implicit env =>
        val picker = DatePicker(value = Some(LocalDate.of(2026, 10, 10))).open()
        val state  = env.comm.take().last.state
        assert(
          state.value.get("value") == Some(
            ujson.Obj("year" -> num(2026), "month" -> num(9), "date" -> num(10))
          )
        )
        env.comm.frontendMessage(
          picker.id,
          update("value" -> ujson.Obj(
            "year"  -> num(2020),
            "month" -> num(0),
            "date"  -> num(31)
          ))
        )
        assert(picker.value() == Some(LocalDate.of(2020, 1, 31)))

        val time = Time(value = Some(LocalTime.of(13, 45, 10, 500000000)))
        assert(
          time.stateValue("value").json == ujson.Obj(
            "hours"        -> num(13),
            "minutes"      -> num(45),
            "seconds"      -> num(10),
            "milliseconds" -> num(500)
          )
        )
        assert(time.step() == Some(60.0))
        time.step() = None
        assert(time.stateValue("step").json == str("any"))
      }
    }

    test("close") {
      withEnv { implicit env =>
        val w = Label().open()
        env.comm.take()
        w.close()
        assert(env.comm.take().map(_.kind) == List("close"))
        assert(env.manager.widget(w.id).isEmpty)
        w.value() = "ignored"
        assert(env.comm.take().isEmpty)
      }
    }

    test("control comm") {
      withEnv { implicit env =>
        val image = Image(value = Array[Byte](1, 2)).open()
        val label = Label(value = "l").open()
        env.comm.take()
        env.comm.frontendOpen(WidgetManager.controlTargetName, "control", ujson.Obj())
        env.comm.frontendMessage("control", ujson.Obj("method" -> str("request_states")))
        val reply = env.comm.take()
        assert(reply.length == 1)
        assert(reply.head.id == "control")
        assert(reply.head.method == Some("update_states"))
        val states = reply.head.data("states").obj
        assert(states.contains(label.id))
        assert(states.contains(image.id))
        val labelState = states(label.id)
        assert(labelState("model_name") == str("LabelModel"))
        assert(labelState("state")("value") == str("l"))
        // binary values are removed from the states
        assert(!states(image.id)("state").obj.contains("value"))
        assert(
          reply.head.bufferPaths == ujson.Arr(ujson.Arr(
            str(image.id),
            str("state"),
            str("value")
          ))
        )
        assert(reply.head.buffers.map(_.toSeq) == Seq(Seq[Byte](1, 2)))
      }
    }

    test("widgets created by the front-end") {
      withEnv { implicit env =>
        val state = ujson.Obj(
          "_model_module"         -> str("@jupyter-widgets/controls"),
          "_model_module_version" -> str("2.0.0"),
          "_model_name"           -> str("ControllerButtonModel"),
          "_view_module"          -> str("@jupyter-widgets/controls"),
          "_view_module_version"  -> str("2.0.0"),
          "_view_name"            -> str("ControllerButtonView"),
          "pressed"               -> ujson.Bool(true),
          "value"                 -> num(0.5)
        )
        env.comm.frontendOpen(
          WidgetManager.targetName,
          "frontend-id",
          ujson.Obj("state" -> state, "buffer_paths" -> ujson.Arr())
        )
        val w = env.manager.widget("frontend-id")
        assert(w.exists(_.isInstanceOf[ControllerButton]))
        val button = w.get.asInstanceOf[ControllerButton]
        assert(button.pressed())
        assert(button.value() == 0.5)
        env.comm.frontendMessage("frontend-id", update("pressed" -> ujson.Bool(false)))
        assert(!button.pressed())

        env.comm.frontendOpen(
          WidgetManager.targetName,
          "other-id",
          ujson.Obj(
            "state" -> ujson.Obj(
              "_model_module" -> str("custom"),
              "_model_name"   -> str("CustomModel"),
              "count"         -> num(2)
            )
          )
        )
        val generic = env.manager.widget("other-id").get.asInstanceOf[GenericWidget]
        assert(generic.modelName == "CustomModel")
        assert(generic.field[Int]("count").apply() == 2)
      }
    }

    test("anywidget") {
      withEnv { implicit env =>
        class Counter extends AnyWidget("export default { render() {} }") {
          val count = prop("count", 0)
        }
        val counter = new Counter
        counter.count() = 2
        counter.open()
        val state = env.comm.take().last.state
        assert(state.value.get("_model_name") == Some(str("AnyModel")))
        assert(state.value.get("_model_module") == Some(str("anywidget")))
        assert(state.value.get("_esm") == Some(str("export default { render() {} }")))
        assert(state.value.get("count") == Some(num(2)))
        assert(!state.value.contains("_css"))
      }
    }

    test("custom widget") {
      withEnv { implicit env =>
        val w = new MyWidget
        w.count() = 3
        w.open()
        val sent = env.comm.take()
        assert(sent.map(_.id) == List(w.layout().id, w.id))
        assert(sent.last.state.value.get("count") == Some(num(3)))
        assert(w.layout().manager eq env.manager)
      }
    }

    test("all widgets") {
      withEnv { implicit env =>
        // creating and opening all the generated widgets, checking their state is valid JSON
        val widgets =
          Seq[Widget](
            Accordion(),
            Audio(),
            BoundedFloatText(),
            BoundedIntText(),
            Box(),
            Button(),
            Checkbox(),
            ColorPicker(),
            ColorsInput(),
            Combobox(),
            Controller(),
            DatePicker(),
            Datetime(value = Some(Instant.EPOCH)),
            Dropdown(options = Seq("a")),
            FileUpload(),
            FloatLogSlider(),
            FloatProgress(),
            FloatRangeSlider(),
            FloatSlider(),
            FloatText(),
            FloatsInput(),
            GridBox(),
            HBox(),
            HTML(),
            HTMLMath(),
            Image(),
            IntProgress(),
            IntRangeSlider(),
            IntSlider(),
            IntText(),
            IntsInput(),
            Label(),
            NaiveDatetime(),
            Output(),
            Password(),
            Play(),
            RadioButtons(options = Seq(1, 2)),
            Select(options = Seq(1, 2)),
            SelectMultiple(options = Seq(1, 2)),
            SelectionRangeSlider(options = Seq(1, 2)),
            SelectionSlider(options = Seq(1, 2)),
            Stack(),
            Tab(),
            TagsInput(),
            Text(),
            Textarea(),
            Time(),
            ToggleButton(),
            ToggleButtons(options = Seq(1, 2)),
            VBox(),
            Valid(),
            Video()
          )
        widgets.foreach(_.open())
        val sent = env.comm.take()
        assert(sent.forall(_.kind == "open"))
        for (msg <- sent)
          assert(ujson.read(msg.data.render()) == msg.data)
        val ids = sent.map(_.id).toSet
        assert(widgets.forall(w => ids.contains(w.id)))
      }
    }
  }
}
