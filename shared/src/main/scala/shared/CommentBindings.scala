package shared

trait CommentBindings {
  def getErrorComment(moduleName: String, errCode: Int): Option[String]
  def getGeneralComment(errMessage: String): Option[String]
}

object CommentBindings {
  def ofMap(errorBindings: Map[(String, Int), String], generalBindings: Map[String, String]): CommentBindings = {
    new CommentBindings {
      def getErrorComment(moduleName: String, errCode: Int): Option[String] = errorBindings.get((moduleName, errCode))
      def getGeneralComment(errMessage: String): Option[String] = generalBindings.get(errMessage)
    }
  }
}
