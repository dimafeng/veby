package com.dimafeng.veby.undertow

import java.io.IOException
import java.nio.ByteBuffer
import java.util.Date

import com.dimafeng.veby.{Action, _}
import io.undertow.Undertow
import io.undertow.server.{HttpHandler, HttpServerExchange, handlers}
import io.undertow.util.HttpString

import scala.collection.JavaConverters._
import scala.concurrent.Promise
import scala.util.{Failure, Success, Try}

object Server {
  def apply[F[_] : CallbackConstructor : Monad](settings: Settings, filters: Filter[F]*)(action: Action[F]): Unit = {
    Undertow.builder
      .addHttpListener(settings.port, settings.host, new Handler(action))
      .build.start()
  }

  case class Settings(port: Int = 8080, host: String = "0.0.0.0")

  object DefaultSettings extends Settings

}

object CallbackConstructor {

  import scala.concurrent.Future

  implicit val future: CallbackConstructor[Future] = new CallbackConstructor[Future] {
    override def apply[T]: Callback[T, Future] = {
      val promise = Promise[T]()

      new Callback[T, Future] {
        override def received(data: T): Unit = promise.success(data)

        override def convert: Future[T] = promise.future
      }
    }
  }
}

class Handler[F[_] : CallbackConstructor : Monad](action: Action[F]) extends HttpHandler {
  override def handleRequest(exchange: HttpServerExchange): Unit = {
    exchange.dispatch { () =>
      val response = action(new UndertowRequest(exchange, implicitly[CallbackConstructor[F]]))
      val monadTypeClass = implicitly[Monad[F]]

      monadTypeClass.flatMap(response) { response =>

        exchange.setStatusCode(response.code)
        response.headers.foreach {
          case (k, v) =>
            exchange.getResponseHeaders.add(new HttpString(k), v)
        }

        response.cookies.values.foreach(c => exchange.setResponseCookie(new CookieAdapter(c)))

        response.data.onData {
          case Data(data) =>
            exchange.getIoThread.execute { () =>
              exchange.getResponseSender.send(ByteBuffer.wrap(data))
            }
          case Tail =>
            exchange.getIoThread.execute { () => exchange.getResponseSender.close() }
        }

        monadTypeClass.unit(())
      }
    }
  }
}

class CookieAdapter(cookie: Cookie) extends handlers.Cookie {
  override def setDiscard(discard: Boolean): Nothing = ???

  override def getName: String = cookie.name

  override def isDiscard: Boolean = cookie.discard

  override def setDomain(domain: String): Nothing = ???

  override def setExpires(expires: Date): Nothing = ???

  override def getMaxAge: Integer = cookie.maxAge

  override def getPath: String = cookie.path

  override def isHttpOnly: Boolean = cookie.httpOnly

  override def getComment: String = ""

  override def getVersion: Int = cookie.version

  override def setVersion(version: Int): Nothing = ???

  override def setMaxAge(maxAge: Integer): Nothing = ???

  override def setPath(path: String): Nothing = ???

  override def isSecure: Boolean = cookie.secure

  override def getDomain: String = cookie.domain

  override def getValue: String = cookie.value

  override def setValue(value: String): Nothing = ???

  override def setHttpOnly(httpOnly: Boolean): Nothing = ???

  override def setComment(comment: String): Nothing = ???

  override def getExpires = cookie.expires

  override def setSecure(secure: Boolean): Nothing = ???
}

trait Callback[T, F[_]] {
  def received(data: T): Unit

  def convert: F[T]
}

trait CallbackConstructor[F[_]] {
  def apply[T]: Callback[T, F]
}

class UndertowRequest[F[_] : Monad](exchange: HttpServerExchange, callbackConstructor: CallbackConstructor[F]) extends Request[F] {
  override def method: String = exchange.getRequestMethod.toString

  override lazy val headers: Map[String, String] = exchange.getRequestHeaders.iterator().asScala
    .map(h => h.getHeaderName.toString -> h.iterator().asScala.mkString(", "))
    .toMap

  override def readBody: F[Try[Array[Byte]]] = {
    val p = callbackConstructor[Try[Array[Byte]]]
    exchange.getRequestReceiver.receiveFullBytes(
      (_: HttpServerExchange, message: Array[Byte]) => {
        p.received(Success(message))
        ()
      },
      (_: HttpServerExchange, ex: IOException) => {
        p.received(Failure(ex))
        ()
      }
    )
    p.convert
  }

  override def readBodyString: F[Try[String]] = {
    val p = callbackConstructor[Try[String]]
    exchange.getRequestReceiver.receiveFullString(
      (_: HttpServerExchange, message: String) => {
        p.received(Success(message))
        ()
      },
      (_: HttpServerExchange, ex: IOException) => {
        p.received(Failure(ex))
        ()
      }
    )
    p.convert
  }

  override def hostName: String = exchange.getHostName

  override def hostPort: Int = exchange.getHostPort

  override def protocol: String = exchange.getProtocol.toString

  override lazy val pathParameters: Map[String, Iterable[String]] =
    exchange.getPathParameters.asScala.map { case (key, value) => key -> value.asScala }.toMap

  override lazy val queryParameters: Map[String, Iterable[String]] =
    exchange.getQueryParameters.asScala.map { case (key, value) => key -> value.asScala }.toMap

  override def queryString: String = exchange.getQueryString

  override lazy val cookies: Map[String, Cookie] =
    exchange.getRequestCookies.asScala.map { case (name, value) => name -> Cookie(
      value.getName,
      value.getValue,
      value.getPath,
      value.getDomain,
      value.getMaxAge,
      value.isDiscard,
      value.isSecure,
      value.isHttpOnly,
      value.getExpires,
      value.getVersion
    )
    }.toMap

  override def charset: String = exchange.getRequestCharset

  override def path: String = exchange.getRequestPath

  override def URL: String = exchange.getRequestURL
}