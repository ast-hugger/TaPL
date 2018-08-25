package tapl.ch17

import tapl.base.Type
import tapl.base.Util._

case class TRecord(fields: Vector[(String, Type)]) extends Type {
  override def toString: String = mapAndJoin(fields)(p => p._1 + ": " + p._2)( _ + ", " + _ )
}
