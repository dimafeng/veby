package com.dimafeng.veby.undertow

import java.io.IOException
import java.nio.ByteBuffer
import java.util.Date

import com.dimafeng.veby._
import io.undertow.Undertow
import io.undertow.server.{HttpHandler, HttpServerExchange, handlers}
import io.undertow.util.HttpString

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

object Server {
  def apply(action: Action, filters: Filter*): Unit = {
    Undertow.builder
      .addHttpListener(8080, "0.0.0.0", new Handler(action))
      //.addHttpListener(8080, "0.0.0.0", new Handler1)
      .build.start()
  }
}

//class Handler1 extends HttpHandler {
//  override def handleRequest(exchange: HttpServerExchange) = {
//
//    exchange.dispatch { () =>
//      new Thread(() => {
//        exchange.getRequestHeaders.put(Headers.CONTENT_TYPE, "application/json")
//        exchange.getResponseSender.send("tet")
//        exchange.endExchange()
//      }).start()
//
//    }
//  }
//}

class Handler(action: Action) extends HttpHandler {
  override def handleRequest(exchange: HttpServerExchange) = {
    exchange.dispatch { () =>
      action(new UndertowRequest(exchange))
        .onComplete {
          case Success(response) =>
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
          case Failure(ex) =>
            ex.printStackTrace()
            ???
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

class UndertowRequest(exchange: HttpServerExchange) extends Request {
  override def method: String = exchange.getRequestMethod.toString

  override lazy val headers: Map[String, String] = exchange.getRequestHeaders.iterator().asScala
    .map(h => h.getHeaderName.toString -> h.iterator().asScala.mkString(", "))
    .toMap

  override def readBody: Future[Array[Byte]] = {
    val p = Promise[Array[Byte]]()
    exchange.getRequestReceiver.receiveFullBytes(
      (_: HttpServerExchange, message: Array[Byte]) => {
        p.success(message)
        ()
      },
      (_: HttpServerExchange, ex: IOException) => {
        p.failure(ex)
        ()
      }
    )
    p.future
  }

  override def readBodyString: Future[String] = {
    val p = Promise[String]()
    exchange.getRequestReceiver.receiveFullString(
      (_: HttpServerExchange, message: String) => {
        p.success(message)
        ()
      },
      (_: HttpServerExchange, ex: IOException) => {
        p.failure(ex)
        ()
      }
    )
    p.future
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