package com.example.zhiruili.js

object RuleModels {

  object Tags {
    val namedRule: String = "named_rule"
    val matchRule: String = "match"
    val appearRule: String = "appear"
    val noAppearRule: String = "no_appear"
    val anyRule: String = "any"
    val allRule: String = "all"
    val notRule: String = "not"
    val seqRule: String = "sequence"
    val ordRule: String = "ordered"
  }

  sealed trait Rule {
    val tag: String
  }
  case class NamedRule(name: String) extends Rule {
    val tag: String = Tags.namedRule
  }
  case class AppearRule(rule: Rule, times: Int) extends Rule {
    val tag: String = Tags.appearRule
  }
  case class SingleRuleRule(tag: String, rule: Rule) extends Rule
  case class RulesListRule(tag: String, rules: List[Rule]) extends Rule
  case class MatchRule(comment: String,
                       level: Option[String],
                       keyTag: Option[String],
                       position: Option[String],
                       message: Option[String],
                       extra: List[(String, String)]) extends Rule {
    val tag: String = Tags.matchRule
  }
}
