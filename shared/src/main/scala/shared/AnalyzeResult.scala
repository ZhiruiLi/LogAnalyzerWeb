package shared

case class AnalyzeResult(relatedLogs: List[LogItem], helpMessage: String, optHelpPage: Option[String])
