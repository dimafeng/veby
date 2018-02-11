package com.dimafeng

package object veby {

  trait Action[F[_]] extends ((Request[F]) => F[Response])

}
