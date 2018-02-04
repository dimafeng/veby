package com.dimafeng.veby

import javax.xml.ws.RequestWrapper

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
        (req: Request) => Future.successful(MockResponse(Map("body" -> req.queryString, "status" -> 200)))) //todo get BodyString from future
    )

    val response1 = Await.result(action(mockedRequest("/test/1234", Get.method)), 1 second).asInstanceOf[MockResponse[Map[String, _]]]
    assert(response1.value("params") == Map("foo" -> List("1234")))
    assert(response1.value("status") == 200)

    val response2 = Await.result(action(mockedRequest("/test/", Get.method)), 1 second).asInstanceOf[MockResponse[Map[String, _]]]
    assert(response2.value("params") == Map())
    assert(response2.value("status") == 200)


    val response3 = Await.result(action(mockedRequest("/test/", "{foo: 'foo'}", Post.method)), 1 second).asInstanceOf[MockResponse[Map[String, _]]]
    assert(response2.value("params") == Map())
    assert(response2.value("status") == 200)

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
    when(request.readBody).thenReturn(Future.successful(body.getBytes))
    request
  }
}

case class MockResponse[T](value: T) extends Response {
  override def data: DataSource = ???

  override def code: Int = ???
}