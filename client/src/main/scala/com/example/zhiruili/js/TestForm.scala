package com.example.zhiruili.js

import com.thoughtworks.binding
import com.thoughtworks.binding.Binding
import org.scalajs.dom.raw.{Element, HTMLElement}

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportTopLevel("TestForm")
@JSExportAll
object TestForm {

  def renderForm(element: Element) = {
    binding.dom.render(element, xxx)
  }

  @binding.dom
  def xxx(): Binding[HTMLElement] = <div>hello world</div>

}
