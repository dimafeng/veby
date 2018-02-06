package com.dimafeng.veby

import com.dimafeng.veby.Response._
import org.scalatest.FlatSpec
import org.scalatest.mockito.MockitoSugar

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

class FilteredActionSpec extends FlatSpec with MockitoSugar {
  it should "apply filters and the order should be correct" in {
    val orderCapturer = ListBuffer[Int]()

    val a: Action = (r) => Future.successful(Ok("test"))

    val f1 = new Filter {
      override def apply(filter: Action)(request: Request): Future[Response] = {
        filter(request).map { r =>
          orderCapturer += 1
          r.withStatusCode(204)
        }
      }
    }

    val f2 = new Filter {
      override def apply(filter: Action)(request: Request): Future[Response] = {
        filter(request).map { r =>
          orderCapturer += 2
          r.withHeaders("test" -> "test")
        }
      }
    }

    val future = new FilteredAction(a, f1, f2).apply(mock[Request])
    val response = Await.result(future, 1 second)

    assert(response.code == 204)
    assert(response.headers == Map("test" -> "test"))
    assert(orderCapturer == ListBuffer(2, 1))
  }
}
