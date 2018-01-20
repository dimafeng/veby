package com.dimafeng.veby

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
        (req: Request) => Future.successful(MockResponse(Map("params" -> req.pathParameters, "route" -> 1)))
      ),
      Get("/test/",
        _ => Future.successful(MockResponse(Map("route" -> 2)))
      )
    )

    val response1 = Await.result(action(mockedRequest("/test/1234")), 1 second).asInstanceOf[MockResponse[Map[String, _]]]
    assert(response1.value("params") == Map("foo" -> 1234))

    val response2 = Await.result(action(mockedRequest("/test/1234")), 1 second).asInstanceOf[MockResponse[Map[String, _]]]
    assert(response2.value("route") == 2)
  }
}

object RouterSpec extends MockitoSugar {
  def mockedRequest(path: String) = {
    val request = mock[Request]
    when(request.path).thenReturn(path)
    request
  }
}

case class MockResponse[T](value: T) extends Response {
  override def data: DataSource = ???

  override def code: Int = ???
}