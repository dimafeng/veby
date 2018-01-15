package com.dimafeng.veby

import scala.concurrent.Future

trait Filter {
  def apply(filter: Action)(request: Request): Future[Response]
}

class FilteredAction(action: Action, filters: Filter*) extends Action {
  override def apply(request: Request): Future[Response] = {
    filters.headOption match {
      case None => action(request)
      case Some(f) => f.apply (new FilteredAction (action, filters.tail: _*) ) (request)
    }
  }
}