package com.dimafeng.veby

trait Filter[F[_]] {
  def apply(filter: Action[F])(request: Request[F]): F[Response]
}

class FilteredAction[F[_]](action: Action[F], filters: Filter[F]*) extends Action[F] {
  override def apply(request: Request[F]): F[Response] = {
    filters.headOption match {
      case None => action(request)
      case Some(f) => f.apply(new FilteredAction(action, filters.tail: _*))(request)
    }
  }
}