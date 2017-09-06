package models

case class DataToAnalyze(platform: String, version: String, problem: Int, start: Option[String], end: Option[String])
