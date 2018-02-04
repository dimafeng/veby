package com.dimafeng.veby

import scala.concurrent.ExecutionContext.Implicits.global
import com.dimafeng.veby.RouterSpec.mockedRequest
import org.mockito.Mockito._
import org.scalatest.FlatSpec
import org.scalatest.mockito.MockitoSugar

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class RouterSpec extends FlatSpec with MockitoSugar {
  it should "TODO" in {
    val action = new Router(
      Get("/test/{foo}",
        (req: Request) => Future.successful(MockResponse(Map("params" -> req.pathParameters, "status" -> 200)))
      ),
      Get("/test/",
        (req: Request) => Future.successful(MockResponse(Map("params" -> req.pathParameters, "status" -> 200)))
      ),
      Post("/test/",
        (req: Request) => req.readBodyString.map(body => MockResponse(Map("body" -> body, "status" -> 200)))),
      Put("/test/",
        (req: Request) => req.readBodyString.map(body => MockResponse(Map("body" -> body, "status" -> 200)))
      ),
      Delete("/test/",
        (req: Request) => req.readBodyString.map(body => MockResponse(Map("body" -> "{success: 'true'}", "status" -> 200)))
      )
    )

    val responseGetWithParams = Await.result(action(mockedRequest("/test/1234", "GET")), 1 second).asInstanceOf[MockResponse[Map[String, _]]]
    assert(responseGetWithParams.value("params") == Map("foo" -> List("1234")))
    assert(responseGetWithParams.value("status") == 200)

    val responseGetWithoutParams = Await.result(action(mockedRequest("/test/", "GET")), 1 second).asInstanceOf[MockResponse[Map[String, _]]]
    assert(responseGetWithoutParams.value("params") == Map())
    assert(responseGetWithoutParams.value("status") == 200)


    val responsePost = Await.result(action(mockedRequest("/test/", "{foo: 'foo'}", "POST")), 1 second).asInstanceOf[MockResponse[Map[String, _]]]
    assert(responsePost.value("body") == "{foo: 'foo'}")
    assert(responsePost.value("status") == 200)

    val responsePut = Await.result(action(mockedRequest("/test/", "{foo: 'foo'}", "PUT")), 1 second).asInstanceOf[MockResponse[Map[String, _]]]
    assert(responsePut.value("body") == "{foo: 'foo'}")
    assert(responsePut.value("status") == 200)

    val responseDelete = Await.result(action(mockedRequest("/test/", "{foo: 'foo'}", "DELETE")), 1 second).asInstanceOf[MockResponse[Map[String, _]]]
    assert(responseDelete.value("body") == "{success: 'true'}")
    assert(responseDelete.value("status") == 200)

  }
}

object RouterSpec extends MockitoSugar {
  def mockedRequest(path: String, method: String): Request = {
    val request = mock[Request]
    when(request.pathParameters).thenReturn(Map.empty[String, Iterable[String]])
    when(request.path).thenReturn(path)
    when(request.method).thenReturn(method)
    request
  }

  def mockedRequest(path: String, body: String, method: String): Request = {
    val request = mock[Request]
    when(request.pathParameters).thenReturn(Map.empty[String, Iterable[String]])
    when(request.path).thenReturn(path)
    when(request.method).thenReturn(method)
    when(request.readBodyString).thenReturn(Future.successful(body))
    request
  }
}

case class MockResponse[T](value: T) extends Response {
  override def data: DataSource = ???

  override def code: Int = ???
}