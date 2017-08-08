package controllers

import java.io.ByteArrayOutputStream
import javax.inject.Inject

import models.DataToAnalyze
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraints
import play.api.i18n.I18nSupport
import play.api.mvc.MultipartFormData.FilePart
import play.api.mvc.{AbstractController, ControllerComponents}
import play.core.parsers.Multipart.FileInfo

import scala.io.Source

class AnalyzerController @Inject()(cc: ControllerComponents) extends AbstractController(cc) with I18nSupport {

  val dataToAnalyze = Form(
    mapping(
      "platform" -> nonEmptyText,
      "version" -> text.verifying(Constraints.pattern("""\d+\.\d+\.\d+""".r)),
      "problem" -> number
    )(DataToAnalyze.apply)(DataToAnalyze.unapply))

  def createForm() = Action { implicit request =>
    Ok(views.html.analyzer.form(dataToAnalyze))
  }

  def post() = Action(parse.multipartFormData) { implicit request =>
    dataToAnalyze.bindFromRequest()
    request.body.file("logfile").map { log =>
      Ok(views.html.analyzer.result("help info 1111", Some("https://www.baidu.com")))
      // Ok(dataToAnalyze.bindFromRequest.toString + "\n" + Source.fromFile(log.ref).mkString)
    }.getOrElse(NotFound)
  }
}
