package com.dimafeng.veby

import java.util.Date

import com.dimafeng.veby.Response.ResponseWrapper

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.reflect.ClassTag

trait Request {
  def method: String

  def headers: Map[String, String]

  def readBody: Future[Array[Byte]]

  def readBodyString: Future[String]

  def readBodyEntity[T: ClassTag](implicit transformer: BodyTransformer): Future[T] = {
    def ctag = implicitly[reflect.ClassTag[T]]

    readBody.map(transformer.as[T](_, ctag.runtimeClass.asInstanceOf[Class[T]]))
  }

  def hostName: String

  def hostPort: Int

  def protocol: String

  def pathParameters: Map[String, Iterable[String]]

  def queryParameters: Map[String, Iterable[String]]

  def queryString: String

  def cookies: Map[String, Cookie]

  def charset: String

  def path: String

  def URL: String

}

object Request {

  class RequestWrapper(request: Request, morePathParameters: Map[String, Iterable[String]] = Map()) extends Request {
    override def method: String = request.method

    override def headers: Map[String, String] = request.headers

    override def readBody: Future[Array[Byte]] = request.readBody

    override def readBodyString: Future[String] = request.readBodyString

    override def hostName: String = request.hostName

    override def hostPort: Int = request.hostPort

    override def protocol: String = request.protocol

    override def pathParameters: Map[String, Iterable[String]] = request.pathParameters ++ morePathParameters // TODO collisions???

    override def queryParameters: Map[String, Iterable[String]] = request.queryParameters

    override def queryString: String = request.queryString

    override def cookies: Map[String, Cookie] = request.cookies

    override def charset: String = request.charset

    override def path: String = request.path

    override def URL: String = request.URL
  }

}

trait BodyTransformer {
  def as[T](data: Array[Byte], toClass: Class[T]): T

  def toBody[T](obj: T): Array[Byte]
}

object BodyTransformer {

  implicit object StringBodyTransformer extends BodyTransformer {
    override def as[T](data: Array[Byte], toClass: Class[T]): T =
      if (toClass == classOf[String]) {
        new String(data).asInstanceOf[T]
      } else {
        ???
      }

    override def toBody[T](obj: T): Array[Byte] = obj match {
      case s: String => s.getBytes
      case _ => ???
    }
  }

}

trait Response {
  def data: DataSource

  def code: Int

  def headers: Map[String, String] = Map()

  def cookies: Map[String, Cookie] = Map()

  def withCookies(cookies: Cookie*): Response = new ResponseWrapper(this, moreCookies = cookies.map(c => c.name -> c).toMap)

  def withHeaders(headers: (String, String)*): Response = new ResponseWrapper(this, moreHeaders = headers.toMap)

  def withContentType(contentType: String): Response = new ResponseWrapper(this, moreHeaders = Map(Headers.CONTENT_TYPE -> contentType))

  def withStatusCode(code: Int): Response = new ResponseWrapper(this, newCode = Some(code))
}

object Response {

  case class Strict(override val data: DataSource,
                    override val code: Int = 200) extends Response

  private class ResponseWrapper(response: Response, moreCookies: Map[String, Cookie] = Map(), moreHeaders: Map[String, String] = Map(), newCode: Option[Int] = None) extends Response {
    override def data: DataSource = response.data

    override val code: Int = newCode.getOrElse(response.code)

    override lazy val headers: Map[String, String] = response.headers ++ moreHeaders

    override lazy val cookies: Map[String, Cookie] = response.cookies ++ moreCookies
  }

  def Ok[T](content: T)(implicit bt: BodyTransformer): Response = Response.Strict(new SingleDataSource(bt.toBody(content)))

  def NotFound(implicit bt: BodyTransformer): Response = Response.Strict(EmptyDataSource, 404)
}

case class Cookie(name: String,
                  value: String,
                  path: String,
                  domain: String,
                  maxAge: Int,
                  discard: Boolean,
                  secure: Boolean,
                  httpOnly: Boolean,
                  expires: Date,
                  version: Int)