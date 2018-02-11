package com.dimafeng.veby

import com.dimafeng.veby.Monad.future
import com.dimafeng.veby.undertow.CallbackConstructor.future
import com.dimafeng.veby.undertow.Server

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Test extends App {

  implicit object MyBodyTransformer extends BodyTransformer {
    override def as[T](data: Array[Byte], toClass: Class[T]): T = ???

    override def toBody[T](obj: T): Array[Byte] = obj.toString.getBytes
  }

  val filter = new Filter[Future] {
    override def apply(filter: Action[Future])(request: Request[Future]): Future[Response] = {
      filter(request).map(_.withStatusCode(300))
    }
  }

  Server(Server.DefaultSettings, filter) { req: Request[Future] =>
    Future {
      Response.Ok(A("1234")).withStatusCode(200)
    }
  }
}


case class A(value: String)
