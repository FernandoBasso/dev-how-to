package UserAuth1

sealed trait User
case class Anonymous(pages: List[String]) extends User
case class LoggedIn(id: Int) extends User
