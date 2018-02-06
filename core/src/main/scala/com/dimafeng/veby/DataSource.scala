package com.dimafeng.veby

trait DataSource {
  def onData(f: PartialFunction[DataFrame, Unit]): Unit
}

class SingleDataSource(data: Array[Byte]) extends DataSource {
  override def onData(f: PartialFunction[DataFrame, Unit]): Unit = {
    f(Data(data))
    f(Tail)
  }
}

object EmptyDataSource extends DataSource {
  override def onData(f: PartialFunction[DataFrame, Unit]): Unit = f(Tail)
}

trait DataFrame

case class Data(data: Array[Byte]) extends DataFrame

object Tail extends DataFrame