package com.example.zhiruili.js

import com.thoughtworks.binding.Binding.{Constants, Var, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.html.{Input, Select}
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.{Event, Node}

import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}
import scala.util.Try

@JSExportTopLevel("RuleEditor")
@JSExportAll
object RuleEditor {

//  implicit def makeIntellijHappy(x: scala.xml.Node): Binding[org.scalajs.dom.raw.Node] = ???

  def editRule(): Unit = {
    startNewEdit(document.getElementById("mainCanvas"))
  }

  def startNewEdit(element: Element): Unit = {
    val root = Var[Rule](NamedRule(Var("")))
    dom.render(element, renderRoot(root))
  }

  object Tags {
    val namedRule = "named_rule"
    val matchRule = "match"
    val appearRule = "appear"
    val noAppearRule = "no_appear"
    val orRule = "or"
    val andRule = "and"
    val notRule = "not"
    val seqRule = "sequence"
    val ordRule = "ordered"
  }

  object Labels {
    val namedRule = "命名规则"
    val matchRule = "匹配日志"
    val appearRule = "任意位置出现"
    val noAppearRule = "不出现"
    val orRule = "满足任一"
    val andRule = "满足全部"
    val notRule = "否定规则"
    val seqRule = "紧密连续出现"
    val ordRule = "松散连续出现"
  }

  def getDefaultRuleByTag(tag: String): Rule = {
    if (tag == Tags.namedRule) NamedRule(Var(""))
    else if(tag == Tags.matchRule) MatchRule(Var(""), Var(None), Var(None), Var(None), Var(None), Vars.empty)
    else if (tag == Tags.appearRule) AppearRule(Var(NamedRule(Var(""))), Var(1))
    else if (tag == Tags.noAppearRule || tag == Tags.notRule) SingleRuleRule(tag, Var(NamedRule(Var(""))))
    else RulesListRule(tag, Vars.empty)
  }

  sealed trait Rule {
    val tag: String
  }
  case class NamedRule(name: Var[String]) extends Rule {
    val tag: String = Tags.namedRule
  }
  case class AppearRule(rule: Var[Rule], times: Var[Int]) extends Rule {
    val tag: String = Tags.appearRule
  }
  case class SingleRuleRule(tag: String, rule: Var[Rule]) extends Rule
  case class RulesListRule(tag: String, rules: Vars[Var[Rule]]) extends Rule
  case class MatchRule(comment: Var[String],
                       level: Var[Option[String]],
                       keyTag: Var[Option[String]],
                       position: Var[Option[String]],
                       message: Var[Option[String]],
                       extra: Vars[(String, String)]) extends Rule {
    val tag: String = Tags.matchRule
  }

  @dom
  def renderRoot(rootRule: Var[Rule]): Binding[HTMLElement] = {
    val convertStr = Var("")
    <div>
      <div>
        { renderSelect(rootRule).bind }
      </div>
      <div>
        <button class="btn my-btn" onclick={ e: Event =>
          val json = ruleToJson(rootRule.value)
          convertStr.value = JSON.stringify(json, space=2)
        }>生成 JSON 字符串</button>
        <pre>{ convertStr.bind } </pre>
      </div>
    </div>
  }

  @dom
  def renderDeleteButton[T](vars: Vars[T], variable: T): Binding[HTMLElement] = {
    val onDelClickHandler = { _: Event =>
      vars.value -= variable
    }
    <button class="btn my-btn" onclick={ onDelClickHandler }>x</button>
  }

  def emptyJObject: js.Dynamic = JSON.parse("{}")

  def createJObject(kvs: List[(String, js.Any)]): js.Dynamic = {
    val json = emptyJObject
    kvs.foreach { case (k, v) => json.updateDynamic(k)(v) }
    json
  }

  def ruleToJson(rule: Rule): js.Dynamic = {
    rule match {
      case NamedRule(name) =>
        JSON.parse(s""""${ name.value }"""")
      case AppearRule(subRule, times) =>
        createJObject(List("type" -> "appear", "times" -> times.value, "rule" -> ruleToJson(subRule.value)))
      case SingleRuleRule(tag, subRule) =>
        createJObject(List("type" -> tag, "rule" -> ruleToJson(subRule.value)))
      case RulesListRule(tag, subRules) =>
        val jArray = js.Array[js.Dynamic](subRules.value.map(_.value).map(ruleToJson).toList: _*)
        createJObject(List("type" -> tag, "rules" -> jArray))
      case MatchRule(comment, optLv, optKey, optPos, optMsg, ext) =>
        val json = createJObject(List("type" -> "match"))
        if(!comment.value.isEmpty) json.comment = comment.value
        optLv.value.foreach(lv => json.level = lv)
        optKey.value.foreach(tag => json.tag = tag)
        optPos.value.foreach(pos => json.position = pos)
        optMsg.value.foreach(msg => json.message = msg)
        def mapToJsVal(kv: (String, String)): (String, js.Any) = (kv._1, kv._2)
        if(ext.value.nonEmpty) json.extra = createJObject(ext.value.toList.map(mapToJsVal))
        json
    }
  }

  @dom
  def renderSelectItem(tag: String, label: String, currSelected: String): Binding[HTMLElement] = {
    <option value={ tag } selected={ tag == currSelected }>{ label }</option>
  }

  @dom
  def renderSelect(rule: Var[Rule]): Binding[HTMLElement] = {
    val onChange = { event: Event =>
      event.currentTarget match {
        case sel: Select => rule.value = getDefaultRuleByTag(sel.value)
      }
    }
    val currTag = rule.value.tag
    <span>
      <select onchange={ onChange } class="my-input-big">
        { renderSelectItem(Tags.namedRule, Labels.namedRule, currTag).bind }
        { renderSelectItem(Tags.matchRule, Labels.matchRule, currTag).bind }
        { renderSelectItem(Tags.appearRule, Labels.appearRule, currTag).bind }
        { renderSelectItem(Tags.noAppearRule, Labels.noAppearRule, currTag).bind }
        { renderSelectItem(Tags.orRule, Labels.orRule, currTag).bind }
        { renderSelectItem(Tags.andRule, Labels.andRule, currTag).bind }
        { renderSelectItem(Tags.notRule, Labels.notRule, currTag).bind }
        { renderSelectItem(Tags.seqRule, Labels.seqRule, currTag).bind }
        { renderSelectItem(Tags.ordRule, Labels.ordRule, currTag).bind }
      </select>
      { renderRule(rule.bind).bind }
    </span>
  }

  def renderRule(rule: Rule): Binding[HTMLElement] = {
    rule match {
      case rule: NamedRule =>
        renderNamedRule(rule)
      case rule: MatchRule =>
        renderMatchRule(rule)
      case rule: AppearRule =>
        renderAppearRule(rule)
      case rule: SingleRuleRule =>
        renderSingleRuleRule(rule)
      case rule: RulesListRule =>
        renderRuleListRule(rule)
    }
  }

  @dom
  def renderInput(text: Var[String], attrClass: String): Binding[HTMLElement] = {
    val onInput = { event: Event =>
      event.currentTarget match {
        case input: Input =>
          text.value = input.value.toString
        case _ => // ignore
      }
    }
    <input type="text" class={ attrClass } oninput={ onInput } value={ text.bind } />
  }

  @dom
  def renderNamedRule(rule: NamedRule): Binding[HTMLElement] = {
    <span>
      { renderInput(rule.name, "my-input-big").bind }
    </span>
  }

  @dom
  def renderCheckableInput(label: String, text: Var[Option[String]], sizeClass: String): Binding[HTMLElement] = {
    var content = ""
    val onCheck = { event: Event =>
      event.currentTarget match {
        case input: Input =>
          if (input.checked) {
            text.value = Some(content)
          } else {
            content = text.value.getOrElse("")
            text.value = None
          }
      }
    }
    val onInput = { event: Event =>
      event.currentTarget match {
        case input: Input => text.value = Some(input.value)
      }
    }
    <span>
      <input type="checkbox" checked={ text.bind.isDefined } onchange={ onCheck } />
      { label }
      <input type="text" class={ sizeClass } disabled={ text.bind.isEmpty } onchange={ onInput } />
    </span>
  }

  @dom
  def renderInputBindings(vars: Vars[(String, String)]): Binding[HTMLElement] = {
    val key = Var("")
    val value = Var("")
    <span>
      {
        for(pair <- vars) yield
        <span>
          <button class="btn my-btn" onclick={ e: Event => vars.value -= pair }>{pair._1} : {pair._2} | x</button>
        </span>
      }
      |
      <span>
        key:{ renderInput(key, "my-input-small").bind }
        val:{ renderInput(value, "my-input-small").bind }
        <button class="btn my-btn"
                onclick={ e: Event => {
                  if (key.value.trim.nonEmpty) {
                    vars.value += (key.value.trim -> value.value.trim)
                    key.value = ""
                    value.value = ""
                  }
                } }
        >+
        </button>
      </span>
    </span>
  }

  @dom
  def renderMatchRule(rule: MatchRule): Binding[HTMLElement] = {
    val showDetail = Var(false)
    val enableLv = Var(false)
    def onChangeHandler(boolVar: Var[Boolean]) = { event: Event =>
      event.currentTarget match {
        case input: Input => boolVar.value = input.checked
        case _ => // ignore
      }
    }
    val onClick = { _: Event => showDetail.value = !showDetail.value }
    <span>
      <button class="btn my-btn" onclick={ onClick }>{ rule.comment.bind } | { if (showDetail.bind) "收起" else "展开" }</button>
      {
        if (showDetail.bind) {
          <span>
            <span>
              备忘名:
              { renderInput(rule.comment, "my-input-middle").bind }
            </span>
            { renderCheckableInput("日志等级:", rule.level, "my-input-xsmall").bind }
            { renderCheckableInput("标签:", rule.keyTag, "my-input-small").bind }
            { renderCheckableInput("打印位置:", rule.position, "my-input-big").bind }
            { renderCheckableInput("日志信息:", rule.message, "my-input-big").bind }
            <span>
              附加信息:
              { renderInputBindings(rule.extra).bind }
            </span>
          </span>
        } else {
          <!-- nothing -->
        }
      }
    </span>
  }

  @dom
  def renderAppearRule(rule: AppearRule): Binding[HTMLElement] = {
    val onInput = { event: Event =>
      event.currentTarget match {
        case input: Input =>
          val inputVal = Try { input.value.toInt }.getOrElse(-1)
          rule.times.value = if (inputVal <= 0) 1 else inputVal
        case _ => // ignore
      }
    }
    <span>
      <input class="my-input-small" type="number" oninput={ onInput } value="1" min="1" /> 次
      { renderSelect(rule.rule).bind }
    </span>
  }

  def renderSingleRuleRule(rule: SingleRuleRule): Binding[HTMLElement] = {
    renderSelect(rule.rule)
  }

  @dom
  def renderRuleListRule(rule: RulesListRule): Binding[HTMLElement] = {
    val onAdd = { _: Event =>
      rule.rules.value += Var(getDefaultRuleByTag(Tags.namedRule))
    }
    <span>
      <ul>
        {
          for(subRule <- rule.rules) yield
            <li>
              { renderSelect(subRule).bind }
              { renderDeleteButton(rule.rules, subRule).bind }
            </li>
        }
        <li><button class="btn" onclick={ onAdd }>添加一条规则</button></li>
      </ul>
    </span>
  }
}
