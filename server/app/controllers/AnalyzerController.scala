package controllers

import java.text.SimpleDateFormat
import java.util.Date
import javax.inject.Inject

import com.example.zhiruili.loganalyzer._
import com.example.zhiruili.loganalyzer.analyzer.LogAnalyzer.{AnalyzeResult, NoSuchProblemException}
import com.example.zhiruili.loganalyzer.analyzer.config.{ConfigLoader, DefaultConfigParser, FileConfigLoader}
import com.example.zhiruili.loganalyzer.analyzer.LogAnalyzerLoader
import com.example.zhiruili.loganalyzer.comment.{CommentBindings, CommentLoader}
import com.example.zhiruili.loganalyzer.logs._
import com.example.zhiruili.loganalyzer.rules.{BasicRuleParser, FileRuleLoader, RuleLoader}
import models.DataToAnalyze
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraints
import play.api.i18n.I18nSupport
import play.api.mvc.{AbstractController, ControllerComponents}

import scala.io.Source
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

class AnalyzerController @Inject()(cc: ControllerComponents, configuration: Configuration) extends AbstractController(cc) with I18nSupport {

  import controllers.AnalyzerController._

  val baseDirPath: String = configuration.get[String]("baseDirPath")
  val configFileName: String = configuration.get[String]("configFileName")
  val genCommentFileName: String = configuration.get[String]("generalCommentFileName")
  val errCommentFileName: String = configuration.get[String]("errorCommentFileName")
  val configLoader: ConfigLoader = FileConfigLoader.createSimpleLoader(baseDirPath, configFileName, DefaultConfigParser)
  val ruleLoader: RuleLoader = FileRuleLoader.createSimpleLoader(baseDirPath, BasicRuleParser)
  val analyzerLoader: LogAnalyzerLoader = LogAnalyzerLoader(configLoader, ruleLoader)
  val logParser: LogParser = LogParser
  val commentLoader: CommentLoader = CommentLoader.ofFile(baseDirPath, errCommentFileName, genCommentFileName)
  val currentSdk: Sdk = ILiveSdk

  val dateTimePattern: Regex = """\s*\d{2}-\d{2}-\d{2}\s+\d{2}:\d{2}:\d{2}\s*""".r
  val dateTimeErrorMsg: String = "日期必须满足 yy-MM-dd HH:mm:ss 的格式"

  val dataToAnalyze = Form(
    mapping(
      "platform" -> text,
      "version" -> text.verifying(Constraints.pattern("""\s*\d+\.\d+\.\d+\s*""".r, "", "版本号必须满足 a.b.c 的格式")),
      "problem" -> number,
      "startTime" -> optional(text.verifying(Constraints.pattern(dateTimePattern, "", dateTimeErrorMsg))),
      "endTime" -> optional(text.verifying(Constraints.pattern(dateTimePattern, "", dateTimeErrorMsg)))
    )(DataToAnalyze.apply)(DataToAnalyze.unapply))

  def createForm() = Action { implicit request =>
    Ok(views.html.analyzer.form(dataToAnalyze))
  }

  def submit() = Action(parse.multipartFormData) { implicit request =>

    val errorFunction = { formWithErrors: Form[DataToAnalyze] =>
      BadRequest(views.html.analyzer.form(formWithErrors))
    }

    val dateTimeFormatter = new SimpleDateFormat("yy-MM-dd HH:mm:ss")

    val successFunction = { data: DataToAnalyze =>
      val startTime = Try(data.start.get).flatMap(s => Try(dateTimeFormatter.parse(s).getTime)).getOrElse(0L)
      val endTime = Try(data.end.get).flatMap(s => Try(dateTimeFormatter.parse(s).getTime)).getOrElse(Long.MaxValue)
      request.body.file("logfile").map { log =>
        (for {
          content <- Try { Source.fromFile(log.ref).mkString }
          logItems <- logParser.parseLogString(content)
          res <- analyzeLog(data.platform, data.version.trim, data.problem, Utils.timeFilter(startTime, endTime)(logItems))
          commentBindings <- commentLoader.loadCommentBindings(currentSdk)
        } yield (res, transformCommentBindings(commentBindings))) match {
          case Failure(UnknownPlatformException(platform)) =>
            Ok(views.html.analyzer.failure(s"找不到指定的平台：$platform"))
          case Failure(NoSuchProblemException(problem)) =>
            Ok(views.html.analyzer.failure(s"没有代号为 $problem 的问题"))
          case Failure(thw) =>
            Ok(views.html.analyzer.failure(s"发生内部错误：$thw"))
          case Success(((Nil, logItems), commentBindings)) =>
            Ok(views.html.analyzer.result(Nil, logItems, commentBindings))
          case Success(((analyzeResult, logItems), commentBindings))=>
            val sharedResult = transformAnalyzeResult(analyzeResult)
            Ok(views.html.analyzer.result(sharedResult, logItems, commentBindings))
        }
      }.getOrElse(Ok(views.html.analyzer.failure(s"分析失败：未上传日志")))
    }

    dataToAnalyze.bindFromRequest.fold(errorFunction, successFunction)
  }

  def analyzeLog(platformString: String, versionString: String, problemCode: Int, logItems: List[LogItem]): Try[(AnalyzeResult, List[shared.LogItem])] = {
    val tryPlatform = platformString match {
      case "android" => Success(PlatformAndroid)
      case "ios" => Success(PlatformIOS)
      case "pc" => Success(PlatformPC)
      case "osx" => Success(PlatformOSX)
      case "web" => Success(PlatformWeb)
      case _ => Failure(UnknownPlatformException(platformString))
    }
    for {
      platform <- tryPlatform
      analyzer <- analyzerLoader.loadAnalyzer(ILiveSdk, platform, versionString)
      res <- analyzer.analyzeLogs(problemCode)(logItems)
    } yield (res, logItems.map(transformLogItem))
  }

  def transformLevel(lv: LogLevel): shared.LogLevel = lv match {
    case LvDebug => shared.LvDebug
    case LvInfo => shared.LvInfo
    case LvWarn => shared.LvWarn
    case LvError => shared.LvError
    case _ => shared.LvUnknown
  }

  def transformLogItem(logItem: LogItem): shared.LogItem = logItem match {
    case LegalLog(time, isKey, lv, pos, msg, ext) =>
      shared.LegalLog(time.getTime, isKey, transformLevel(lv), pos, msg, ext)
    case UnknownLog(log) => shared.UnknownLog(log)
  }

//  def transformAnalyzeResult(analyzeResult: AnalyzeResult): List[shared.AnalyzeResult] = {
//    analyzeResult.map { case (logItems, helpInfo) =>
//        shared.AnalyzeResult(logItems.map(transformLogItem), helpInfo.message, helpInfo.helpPage)
//    }
//  }

  def transformAnalyzeResult(analyzeResult: AnalyzeResult): List[String] = {
    analyzeResult.map { case (logItems, helpInfo) =>
        shared.AnalyzeResult(logItems.map(transformLogItem), helpInfo.message, helpInfo.helpPage)
    }
  }


  def transformCommentBindings(commentBindings: CommentBindings): shared.CommentBindings = {
    shared.CommentBindings.ofMap(commentBindings.errorBindings.toMap, commentBindings.generalBindings.toMap)
  }
}

object AnalyzerController {

  case class UnknownPlatformException(platformName: String) extends RuntimeException(s"Unknown platform: $platformName")

  private object Formatter {

    val dateFormatter = new SimpleDateFormat("HH:mm:ss")

    def formatDate(date: Date): String = dateFormatter.format(date)

    def formatIsKey(isKeyLog: Boolean): String = if(isKeyLog) "*" else ""

    def formatLevel(lv: LogLevel): String = lv match {
      case LvDebug => "D"
      case LvInfo => "I"
      case LvWarn => "W"
      case LvError => "E"
    }

    def formatExt(ext: Map[String, String]): String = {
      ext
        .toList
        .map { case (k, v) => k + " -> " + v }
        .mkString(", ")
    }
  }

  def formatLog(logItem: LogItem): String = logItem match {
    case LegalLog(time, isKey, lv, pos, msg, ext) =>
      s"${Formatter.formatIsKey(isKey)}[${Formatter.formatDate(time)}] $msg (${Formatter.formatExt(ext)}) [$pos][${Formatter.formatLevel(lv)}]"
    case UnknownLog(originalLog) => originalLog
  }

  private val levelColorMap: Map[LogLevel, String] =
    Map(LvError -> "red", LvWarn -> "orange", LvInfo -> "green", LvDebug -> "black")

  val defaultColor: String = "#666666"

  def logColor(log: LogItem): String = log match {
    case log: LegalLog => levelColorMap.getOrElse(log.level, defaultColor)
    case _ => defaultColor
  }
}
