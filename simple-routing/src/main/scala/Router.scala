package com.dimafeng.veby

import java.util.regex.Pattern

import com.dimafeng.veby.Request.RequestWrapper

import scala.concurrent.Future

/**
  * Naive implementation of routing
  */
class Router(routes: Route*) extends Action {
  private val compiledRoutes = routes.map(Router.createCompiledRoute)

  override def apply(req: Request): Future[Response] = {
    compiledRoutes.find(r =>
      req.method.toLowerCase == r.route.method && (r.route.pattern == "*" || r.pattern.matcher(req.path).matches())
    ).map { r =>
      val matcher = r.pattern.matcher(req.path)
      val parameters: Map[String, Iterable[String]] = if (matcher.find()) {
        r.parameterNames.map(name => name -> Iterable(matcher.group(name))).toMap
      } else {
        Map()
      }
      r.route.action(new RequestWrapper(req, parameters))
    }.getOrElse(???) // TODO default 404
  }
}

object Router {
  private val parameterRegex = "{([^}]*?)}".r

  def createCompiledRoute(route: Route): CompiledRoute = {
    val pattern = parameterRegex.replaceAllIn(route.pattern, "(?<$1>\\\\[^/]+)")
    val parameterNames = parameterRegex.findAllMatchIn(route.pattern).map(_.group(1)).toSeq

    CompiledRoute(Pattern.compile(pattern), parameterNames, route)
  }

  private case class CompiledRoute(pattern: Pattern, parameterNames: Seq[String], route: Route)

}

sealed trait Route {
  def method: String

  def pattern: String

  def action: Action
}

case class Get(pattern: String, action: Action) extends Route {
  val method = "GET"
}