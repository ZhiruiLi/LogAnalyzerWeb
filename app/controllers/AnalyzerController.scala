package controllers

import java.text.SimpleDateFormat
import java.util.Date
import javax.inject.Inject

import com.example.zhiruili.loganalyzer._
import com.example.zhiruili.loganalyzer.analyzer.LogAnalyzer.NoSuchProblemException
import com.example.zhiruili.loganalyzer.analyzer.config.ConfigParser.HelpInfo
import com.example.zhiruili.loganalyzer.analyzer.{LogAnalyzerLoader, SimpleLogAnalyzerLoader}
import com.example.zhiruili.loganalyzer.logs._
import controllers.AnalyzerController._
import models.DataToAnalyze
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraints
import play.api.i18n.I18nSupport
import play.api.mvc.{AbstractController, ControllerComponents}

import scala.io.Source
import scala.util.{Failure, Success, Try}

class AnalyzerController @Inject()(cc: ControllerComponents) extends AbstractController(cc) with I18nSupport {

  val dataToAnalyze = Form(
    mapping(
      "platform" -> text.verifying(Constraints.pattern(".+".r, "", "请提供运行平台")),
      "version" -> text.verifying(Constraints.pattern("""\d+\.\d+\.\d+""".r, "", "版本号必须满足 a.b.c 的格式")),
      "problem" -> number
    )(DataToAnalyze.apply)(DataToAnalyze.unapply))

  def createForm() = Action { implicit request =>
    Ok(views.html.analyzer.form(dataToAnalyze))
  }

  def submit() = Action(parse.multipartFormData) { implicit request =>

    val errorFunction = { formWithErrors: Form[DataToAnalyze] =>
      BadRequest(views.html.analyzer.form(formWithErrors))
    }

    val successFunction = { data: DataToAnalyze =>
      val trySdk = Success(ILiveSdk)
      val tryPlt = data.platform match {
        case "android" => Success(PlatformAndroid)
        case "ios" => Success(PlatformIOS)
        case "pc" => Success(PlatformPC)
        case "osx" => Success(PlatformOSX)
        case "web" => Success(PlatformWeb)
        case _ => Failure(UnknownPlatformException(data.platform))
      }
      val tryVer = Success(data.version)
      lazy val tryFileStr = for {
        log <- Try { request.body.file("logfile").get }
        content <- Try { Source.fromFile(log.ref).mkString }
      } yield content
      val analyzeRes = for {
        sdk <- trySdk
        platform <- tryPlt
        version <- tryVer
        analyzer <- analyzerLoader.loadAnalyzer(sdk, platform, version)
        content <- tryFileStr
        logItems <- logParser.parseLogString(content)
        res <- analyzer.analyzeLogs(data.problem)(logItems)
      } yield res
      analyzeRes match {
        case Failure(UnknownPlatformException(platform)) =>
          Ok(views.html.analyzer.result(List((s"找不到指定的平台：$platform", None, Nil))))
        case Failure(NoSuchProblemException(problem)) =>
          Ok(views.html.analyzer.result(List((s"没有代号为 '$problem' 的问题", None, Nil))))
        case Failure(thw) =>
          Ok(views.html.analyzer.result(List((s"分析失败：$thw", None, Nil))))
        case Success(Nil) =>
          Ok(views.html.analyzer.result(List(("未能分析出相关问题", Some("https://www.qcloud.com/document/product/268/7752"), Nil))))
        case Success(analyzeResult) =>
          val resultToShow = analyzeResult.map {
            case (matchLogs, HelpInfo(helpMsg, optHelpPage)) =>
              (helpMsg, optHelpPage, matchLogs.map(log => (logColor(log), formatLog(log))))
          }
          Ok(views.html.analyzer.result(resultToShow))
      }
    }

    dataToAnalyze.bindFromRequest.fold(errorFunction, successFunction)
  }
}

object AnalyzerController {

  val analyzerLoader: LogAnalyzerLoader = SimpleLogAnalyzerLoader("/Users/zhiruili/Projects/temp/AnalyzerBase", "_init_.json")

  val logParser: LogParser = LogParser

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

  private val levelColorMap: Map[LogLevel, String] = Map(LvError -> "red", LvWarn -> "yellow", LvInfo -> "green")

  val defaultColor: String = "black"

  def logColor(log: LogItem): String = log match {
    case log: LegalLog => levelColorMap.getOrElse(log.level, defaultColor)
    case _ => defaultColor
  }
}
