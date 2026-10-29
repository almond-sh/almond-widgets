package widgetsbuild

object Versions {
  // The almond version we build against. Widgets rely on its API (almond.display.ResultDisplay,
  // JupyterApi.currentOpt, the comm handler), which kernels keep binary compatible across their
  // later versions. We pin a timestamped snapshot of almond, so that builds don't change when new
  // 0.15.1-SNAPSHOT builds get published.
  def almond = "0.15.1-20261029.190142-82"

  // widget classes are generated from the model schema of that version (see WidgetModelSchema)
  def ipywidgets = "8.1.7"
  // checksum of the model schema of ipywidgets, to update along with its version
  def ipywidgetsSchemaSha256 = "9b74e3760501675fab399e0c8f0339327296d2308482d5c739cc5c4b60abf104"
}
