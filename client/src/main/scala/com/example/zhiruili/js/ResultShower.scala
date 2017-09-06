package com.example.zhiruili.js

import org.scalajs.dom
import org.scalajs.dom.raw.HTMLElement

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportTopLevel("ResultShower")
@JSExportAll
object ResultShower {

  def renderResult(helpContainer: HTMLElement,
                   logContainer: HTMLElement): Unit = {

    dom.console.log("hello")

    helpContainer.innerHTML = s"""hello $analyzeResults"""
    logContainer.innerHTML = s"""world $commentBindings"""

  }

}
