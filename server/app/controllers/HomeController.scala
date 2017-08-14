package controllers

import javax.inject._

import play.api.i18n.I18nSupport
import play.api.mvc._

@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc) with I18nSupport {

  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def editRule() = Action { implicit request =>
    Ok(views.html.analyzer.ruleeditor())
  }

  def testForm() = Action { implicit request =>
    Ok(views.html.testform())
  }
}
