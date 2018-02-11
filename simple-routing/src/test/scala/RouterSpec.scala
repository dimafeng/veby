package com.dimafeng.veby

import com.dimafeng.veby.RouterSpec.{mockedRequest, effect}
import org.mockito.Mockito._
import org.scalatest.FlatSpec
import org.scalatest.mockito.MockitoSugar

class RouterSpec extends FlatSpec with MockitoSugar {
  it should "process routes correctly" in {
    val action = new Router[Option](
      GET("/test/{foo}",
        (req: Request[Option]) => Some(MockResponse(Map("params" -> req.pathParameters)))
      ),
      GET("/test/",
        (req: Request[Option]) => Some(MockResponse(Map("params" -> req.pathParameters)))
      ),
      POST("/test/",
        (req: Request[Option]) => req.readBodyString.map(body => MockResponse(Map("body" -> body)))),
      PUT("/test/",
        (req: Request[Option]) => req.readBodyString.map(body => MockResponse(Map("body" -> body)))
      ),
      DELETE("/test/",
        (req: Request[Option]) => req.readBodyString.map(body => MockResponse(Map("body" -> "{success: 'true'}")))
      )
    )

    val responseGetWithParams = action(mockedRequest("/test/1234", "GET")).get.asInstanceOf[MockResponse[Map[String, _]]]
    assert(responseGetWithParams.value("params") == Map("foo" -> List("1234")))

    val responseGetWithoutParams = action(mockedRequest("/test/", "GET")).get.asInstanceOf[MockResponse[Map[String, _]]]
    assert(responseGetWithoutParams.value("params") == Map())

    val responsePost = action(mockedRequest("/test/", "{foo: 'foo'}", "POST")).get.asInstanceOf[MockResponse[Map[String, _]]]
    assert(responsePost.value("body") == "{foo: 'foo'}")

    val responsePut = action(mockedRequest("/test/", "{foo: 'foo'}", "PUT")).get.asInstanceOf[MockResponse[Map[String, _]]]
    assert(responsePut.value("body") == "{foo: 'foo'}")

    val responseDelete = action(mockedRequest("/test/", "{foo: 'foo'}", "DELETE")).get.asInstanceOf[MockResponse[Map[String, _]]]
    assert(responseDelete.value("body") == "{success: 'true'}")

    val responseNotFound = action(mockedRequest("/test-not-found/", "{foo: 'foo'}", "DELETE")).get
    assert(responseNotFound.code == 404)
  }
}

object RouterSpec extends MockitoSugar {

  implicit val effect: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](option: Option[A])(f: A => Option[B]): Option[B] = option.flatMap(f)

    override def unit[A](a: A): Option[A] = Some(a)
  }

  def mockedRequest(path: String, method: String): Request[Option] = {
    val request = mock[Request[Option]]
    when(request.pathParameters).thenReturn(Map.empty[String, Iterable[String]])
    when(request.path).thenReturn(path)
    when(request.method).thenReturn(method)
    request
  }

  def mockedRequest(path: String, body: String, method: String): Request[Option] = {
    val request = mock[Request[Option]]
    when(request.pathParameters).thenReturn(Map.empty[String, Iterable[String]])
    when(request.path).thenReturn(path)
    when(request.method).thenReturn(method)
    when(request.readBodyString).thenReturn(Some(Right(body)))
    request
  }
}

case class MockResponse[T](value: T) extends Response {
  override def data: DataSource = ???

  override def code: Int = ???
}