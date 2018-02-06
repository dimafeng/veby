package com.dimafeng.veby

import java.util.regex.Pattern

import com.dimafeng.veby.Request.RequestWrapper
import com.dimafeng.veby.Response._

import scala.concurrent.Future

/**
  * Naive implementation of routing
  */
class Router(routes: Route*) extends Action {
  private val compiledRoutes = routes.map(Router.createCompiledRoute)

  override def apply(req: Request): Future[Response] = {
    compiledRoutes.find(r =>
      req.method.toUpperCase == r.route.method && (r.route.pattern == "*" || r.pattern.matcher(req.path).matches())
    ).map { r =>
      val matcher = r.pattern.matcher(req.path)
      val parameters: Map[String, Iterable[String]] = if (matcher.find()) {
        r.parameterNames.map(name => name -> Iterable(matcher.group(name))).toMap
      } else {
        Map()
      }
      r.route.action(new RequestWrapper(req, parameters))
    }.getOrElse(Future.successful(NotFound))
  }
}

object Router {
  private val parameterRegex = "\\{([^}]*?)\\}".r

  def createCompiledRoute(route: Route): CompiledRoute = {
    val pattern = parameterRegex.replaceAllIn(route.pattern, "(?<$1>[^/]+)")
    val parameterNames = parameterRegex.findAllMatchIn(route.pattern).map(_.group(1)).toSeq

    CompiledRoute(Pattern.compile(pattern), parameterNames, route)
  }

  protected case class CompiledRoute(pattern: Pattern, parameterNames: Seq[String], route: Route)

}

trait Route {
  def method: String

  def pattern: String

  def action: Action
}

case class GET(pattern: String, action: Action) extends Route {
  val method: String = "GET"
}

case class POST(pattern: String, action: Action) extends Route {
  val method: String = "POST"
}

case class DELETE(pattern: String, action: Action) extends Route {
  val method: String = "DELETE"
}

case class PUT(pattern: String, action: Action) extends Route {
  val method: String = "PUT"
}

case class HEAD(pattern: String, action: Action) extends Route {
  val method: String = "HEAD"
}

case class OPTIONS(pattern: String, action: Action) extends Route {
  val method: String = "OPTIONS"
}

case class CONNECT(pattern: String, action: Action) extends Route {
  val method: String = "CONNECT"
}

