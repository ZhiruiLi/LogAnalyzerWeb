package com.example.zhiruili.js

import org.scalajs.dom._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

object JSMain extends js.JSApp {

  def main(): Unit = { }

  def appendText(targetNode: Node, text: String): Unit = {
    val parNode = document.createElement("p")
    val textNode = document.createTextNode(text)
    parNode.appendChild(textNode)
    targetNode.appendChild(parNode)
  }
}
