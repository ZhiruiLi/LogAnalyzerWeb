package com.example.zhiruili.js.rule

import com.example.zhiruili.js.rule.RuleModels._

import scala.scalajs.js
import scala.scalajs.js.JSON

object RuleFormatter {

  def createJObject(kvs: List[(String, js.Any)]): js.Dictionary[js.Any] = {
    val json = js.Dictionary.empty[js.Any]
    kvs.foreach { case (k, v) => json(k) = v }
    json
  }

  def ruleToJson(rule: Rule): js.Any = {
    rule match {
      case NamedRule(name) =>
        name
      case AppearRule(subRule, times) =>
        createJObject(List("type" -> "appear", "times" -> times, "rule" -> ruleToJson(subRule)))
      case SingleRuleRule(tag, subRule) =>
        createJObject(List("type" -> tag, "rule" -> ruleToJson(subRule)))
      case RulesListRule(tag, subRules) =>
        val jArray = js.Array[js.Any](subRules.map(ruleToJson): _*)
        createJObject(List("type" -> tag, "rules" -> jArray))
      case MatchRule(comment, optLv, optKey, optPos, optMsg, ext) =>
        val json = createJObject(List("type" -> "match"))
        if(!comment.isEmpty) json("comment") = comment
        optLv.foreach(lv => json("level") = lv)
        optKey.foreach(tag => json("tag") = tag)
        optPos.foreach(pos => json("position") = pos)
        optMsg.foreach(msg => json("message") = msg)
        def mapToJsVal(kv: (String, String)): (String, js.Any) = (kv._1, kv._2)
        if(ext.nonEmpty) json("extra") = createJObject(ext.map(mapToJsVal))
        json
    }
  }

  def formatJsonString(rule: Rule, space: Int): String = {
    JSON.stringify(ruleToJson(rule), space = space)
  }
}
