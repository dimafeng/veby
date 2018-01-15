package com.dimafeng

import scala.concurrent.Future

package object veby {
  type Action = (Request) => Future[Response]
}
