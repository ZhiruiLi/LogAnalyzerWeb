package com.example.zhiruili.js

import com.example.zhiruili.js.RuleModels._

import scala.scalajs.js
import scala.scalajs.js.JSON

object RuleFormatter {

  def emptyJObject: js.Dynamic = JSON.parse("{}")

  def createJObject(kvs: List[(String, js.Any)]): js.Dynamic = {
    val json = emptyJObject
    kvs.foreach { case (k, v) => json.updateDynamic(k)(v) }
    json
  }

  def ruleToJson(rule: Rule): js.Dynamic = {
    rule match {
      case NamedRule(name) =>
        JSON.parse(s""""${ name }"""")
      case AppearRule(subRule, times) =>
        createJObject(List("type" -> "appear", "times" -> times, "rule" -> ruleToJson(subRule)))
      case SingleRuleRule(tag, subRule) =>
        createJObject(List("type" -> tag, "rule" -> ruleToJson(subRule)))
      case RulesListRule(tag, subRules) =>
        val jArray = js.Array[js.Dynamic](subRules.map(ruleToJson): _*)
        createJObject(List("type" -> tag, "rules" -> jArray))
      case MatchRule(comment, optLv, optKey, optPos, optMsg, ext) =>
        val json = createJObject(List("type" -> "match"))
        if(!comment.isEmpty) json.comment = comment
        optLv.foreach(lv => json.level = lv)
        optKey.foreach(tag => json.tag = tag)
        optPos.foreach(pos => json.position = pos)
        optMsg.foreach(msg => json.message = msg)
        def mapToJsVal(kv: (String, String)): (String, js.Any) = (kv._1, kv._2)
        if(ext.nonEmpty) json.extra = createJObject(ext.toList.map(mapToJsVal))
        json
    }
  }

  def formatJsonString(rule: Rule, space: Int): String = {
    JSON.stringify(ruleToJson(rule), space = space)
  }
}
