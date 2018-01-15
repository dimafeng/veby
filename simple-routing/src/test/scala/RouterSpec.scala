package com.dimafeng.veby

import org.scalatest.FlatSpec
import org.scalatest.mockito.MockitoSugar

import scala.concurrent.Future

class RouterSpec extends FlatSpec with MockitoSugar {
  it should "TODO" in {
    new Router(
      Get("/test/{foo}",
        (req: Request) => Future.successful(Ok("test"))
      ),
      Get("/test/",
        _ => Future.successful(Ok("test"))
      )
    )
  }
}
