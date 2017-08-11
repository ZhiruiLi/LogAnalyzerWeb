package example

import org.scalajs.dom
import shared.SharedMessages

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

object ScalaJSExample extends js.JSApp {

  def main(): Unit = {
    // dom.document.getElementById("scalajs").textContent = SharedMessages.itWorks
  }

  @JSExportTopLevel("aaa")
  def aaa(): Unit = {
    dom.document.getElementById("scalajs").textContent = SharedMessages.itWorks
  }
}
