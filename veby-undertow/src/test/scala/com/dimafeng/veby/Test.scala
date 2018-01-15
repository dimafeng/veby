package com.dimafeng.veby

import com.dimafeng.veby.undertow.Server

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Test extends App {

  //Server(res => res.readBodyString.flatMap(b => Ok(b)))
  implicit object MyBodyTransformer extends BodyTransformer {
    override def as[T](data: Array[Byte], toClass: Class[T]): T = ???

    override def toBody[T](obj: T): Array[Byte] = obj.toString.getBytes
  }

  val filter = new Filter {
    override def apply(filter: Action)(request: Request): Future[Response] = {
      filter(request).map(_.withStatusCode(300))
    }
  }

  Server(res => Future {
    println(res.path)
    println(res.URL)
    Ok(A("1234")).withStatusCode(205)
  })
}


case class A(value: String)
