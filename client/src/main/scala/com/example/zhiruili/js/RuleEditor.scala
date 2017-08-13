package com.example.zhiruili.js

import com.example.zhiruili.js.RuleModels.Tags
import com.thoughtworks.binding.Binding.{Var, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.html.{Input, Select}
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.Event

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}
import scala.util.Try

@JSExportTopLevel("RuleEditor")
@JSExportAll
object RuleEditor {

//  implicit def makeIntellijHappy(x: scala.xml.Node): Binding[HTMLElement] = ???

  object BindingRules {
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
  }

  implicit def asBindingRule(rule: RuleModels.Rule): BindingRuleConverter = BindingRuleConverter(rule)

  case class BindingRuleConverter(rule: RuleModels.Rule) {
    def toBinding: Var[BindingRules.Rule] = Var(rule match {
      case RuleModels.NamedRule(name) =>
        BindingRules.NamedRule(Var(name))
      case RuleModels.MatchRule(comment, level, keyTag, pos, msg, ext) =>
        BindingRules.MatchRule(Var(comment), Var(level), Var(keyTag), Var(pos), Var(msg), Vars(ext: _*))
      case RuleModels.AppearRule(rule, times) =>
        BindingRules.AppearRule(rule.toBinding, Var(times))
      case RuleModels.SingleRuleRule(tag, rule) =>
        BindingRules.SingleRuleRule(tag, rule.toBinding)
      case RuleModels.RulesListRule(tag, rules) =>
        BindingRules.RulesListRule(tag, Vars(rules.map(_.toBinding): _*))
    })
  }

  implicit def asStableRule(rule: Var[BindingRules.Rule]): StableRuleConverter = StableRuleConverter(rule)

  case class StableRuleConverter(rule: Var[BindingRules.Rule]) {
    def toStable: RuleModels.Rule = rule.value match {
      case BindingRules.NamedRule(nameVar) =>
        RuleModels.NamedRule(nameVar.value)
      case BindingRules.MatchRule(comVar, lvVar, tagVar, posVar, msgVar, extVar) =>
        RuleModels.MatchRule(comVar.value, lvVar.value, tagVar.value, posVar.value, msgVar.value, extVar.value.toList)
      case BindingRules.AppearRule(ruleVar, timesVar) =>
        RuleModels.AppearRule(ruleVar.toStable, timesVar.value)
      case BindingRules.SingleRuleRule(tag, ruleVar) =>
        RuleModels.SingleRuleRule(tag, ruleVar.toStable)
      case BindingRules.RulesListRule(tag, rulesVars) =>
        RuleModels.RulesListRule(tag, rulesVars.value.toList.map(_.toStable))
    }
  }

  import BindingRules._

  def editRule(): Unit = {
    startNewEdit(document.getElementById("mainCanvas"))
  }

  def startNewEdit(element: Element): Unit = {
    dom.render(element, renderRoot(Var(NamedRule(Var("")))))
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

  def genRuleByTag(tag: String): Rule = {
    if (tag == Tags.namedRule) NamedRule(Var(""))
    else if(tag == Tags.matchRule) MatchRule(Var("未命名"), Var(None), Var(None), Var(None), Var(None), Vars.empty)
    else if (tag == Tags.appearRule) AppearRule(Var(NamedRule(Var(""))), Var(1))
    else if (tag == Tags.noAppearRule || tag == Tags.notRule) SingleRuleRule(tag, Var(NamedRule(Var(""))))
    else RulesListRule(tag, Vars.empty)
  }

  @dom
  def renderRoot(rootRule: Var[Rule]): Binding[HTMLElement] = {
    val convertStr = Var("")
    <div>
      <div>{ renderSelect(rootRule).bind }</div>
      <div>
        <button class="btn my-btn" onclick={ _: Event =>
          convertStr.value = RuleFormatter.formatJsonString(rootRule.toStable, 2)
        }>预览配置文件</button>
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

  @dom
  def renderSelectItem(tag: String, label: String, currSelected: String): Binding[HTMLElement] = {
    <option value={ tag } selected={ tag == currSelected }>{ label }</option>
  }

  @dom
  def renderSelect(rule: Var[Rule]): Binding[HTMLElement] = {
    val onChange = { event: Event =>
      event.currentTarget match {
        case sel: Select =>
          val newRuleVal = (rule.value, genRuleByTag(sel.value)) match {
            case (oldRule: RulesListRule, newRule: RulesListRule) =>
              newRule.copy(rules = oldRule.rules)
            case (oldRule: SingleRuleRule, newRule: SingleRuleRule) =>
              newRule.copy(rule = oldRule.rule)
            case (oldRule: RulesListRule, newRule: SingleRuleRule) =>
              oldRule.rules.value.headOption.map(r => newRule.copy(rule = r)).getOrElse(newRule)
            case (oldRule: SingleRuleRule, newRule: RulesListRule) =>
              newRule.copy(rules = Vars(oldRule.rule))
            case (oldRule: SingleRuleRule, newRule: AppearRule) =>
              newRule.copy(rule = oldRule.rule)
            case (oldRule: AppearRule, newRule: SingleRuleRule) =>
              newRule.copy(rule = oldRule.rule)
            case (oldRule: RulesListRule, newRule: AppearRule) =>
              oldRule.rules.value.headOption.map(r => newRule.copy(rule = r)).getOrElse(newRule)
            case (oldRule: AppearRule, newRule: RulesListRule) =>
              newRule.copy(rules = Vars(oldRule.rule))
            case (_, newRule) => newRule
          }
          rule.value = newRuleVal
      }
    }
    val onWrap = { _: Event =>
      rule.value = AppearRule(Var(rule.value), Var(1))
    }
    val onUnwrap = { _: Event =>
      rule.value match {
        case AppearRule(subRule, _) => rule.value = subRule.value
        case SingleRuleRule(_, subRule) => rule.value = subRule.value
        case RulesListRule(_, subRules) => subRules.value.headOption.foreach(r => rule.value = r.value)
        case _ => // ignore
      }
    }
    <span>
      <button class="btn my-btn" onclick={ onWrap }>+</button>
      <button class="btn my-btn" onclick={ onUnwrap }>-</button>
      <select onchange={ onChange } class="my-input-big">
        { renderSelectItem(Tags.namedRule, Labels.namedRule, rule.bind.tag).bind }
        { renderSelectItem(Tags.matchRule, Labels.matchRule, rule.bind.tag).bind }
        { renderSelectItem(Tags.appearRule, Labels.appearRule, rule.bind.tag).bind }
        { renderSelectItem(Tags.noAppearRule, Labels.noAppearRule, rule.bind.tag).bind }
        { renderSelectItem(Tags.anyRule, Labels.orRule, rule.bind.tag).bind }
        { renderSelectItem(Tags.allRule, Labels.andRule, rule.bind.tag).bind }
        { renderSelectItem(Tags.notRule, Labels.notRule, rule.bind.tag).bind }
        { renderSelectItem(Tags.seqRule, Labels.seqRule, rule.bind.tag).bind }
        { renderSelectItem(Tags.ordRule, Labels.ordRule, rule.bind.tag).bind }
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
          <button class="btn my-btn" onclick={ _: Event => vars.value -= pair }>{pair._1} : {pair._2} | x</button>
        </span>
      }
      |
      <span>
        键:{ renderInput(key, "my-input-small").bind }
        值:{ renderInput(value, "my-input-small").bind }
        <button class="btn my-btn"
                onclick={ _: Event => {
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
    val onClick = { _: Event => showDetail.value = !showDetail.value }
    <span>
      <button class="btn my-btn" onclick={ onClick }>{ rule.comment.bind } | { if (showDetail.bind) "收起" else "展开" }</button>
      {
        if (showDetail.bind) {
          <span>
            <span>
              备忘名:
              { renderInput(rule.comment, "my-input-big").bind }
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
      rule.rules.value += Var(genRuleByTag(Tags.namedRule))
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
