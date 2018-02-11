package com.dimafeng.veby

import java.util.regex.Pattern

import com.dimafeng.veby.Request.RequestWrapper
import com.dimafeng.veby.Response._

/**
  * Naive implementation of routing
  */
class Router[F[_] : Monad](routes: Route[F]*) extends Action[F] {
  private val compiledRoutes = routes.map(Router.createCompiledRoute)

  override def apply(req: Request[F]): F[Response] = {
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
    }.getOrElse(implicitly[Monad[F]].unit(NotFound))
  }
}

object Router {
  private val parameterRegex = "\\{([^}]*?)\\}".r

  def createCompiledRoute[F[_]](route: Route[F]): CompiledRoute[F] = {
    val pattern = parameterRegex.replaceAllIn(route.pattern, "(?<$1>[^/]+)")
    val parameterNames = parameterRegex.findAllMatchIn(route.pattern).map(_.group(1)).toSeq

    CompiledRoute[F](Pattern.compile(pattern), parameterNames, route)
  }

  protected case class CompiledRoute[F[_]](pattern: Pattern, parameterNames: Seq[String], route: Route[F])

}

trait Route[F[_]] {
  def method: String

  def pattern: String

  def action: Action[F]
}

case class GET[F[_]](pattern: String, action: Action[F]) extends Route[F] {
  val method: String = "GET"
}

case class POST[F[_]](pattern: String, action: Action[F]) extends Route[F] {
  val method: String = "POST"
}

case class DELETE[F[_]](pattern: String, action: Action[F]) extends Route[F] {
  val method: String = "DELETE"
}

case class PUT[F[_]](pattern: String, action: Action[F]) extends Route[F] {
  val method: String = "PUT"
}

case class HEAD[F[_]](pattern: String, action: Action[F]) extends Route[F] {
  val method: String = "HEAD"
}

case class OPTIONS[F[_]](pattern: String, action: Action[F]) extends Route[F] {
  val method: String = "OPTIONS"
}

case class CONNECT[F[_]](pattern: String, action: Action[F]) extends Route[F] {
  val method: String = "CONNECT"
}

