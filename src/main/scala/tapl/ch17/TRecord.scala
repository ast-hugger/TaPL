package tapl.ch17

import tapl.base.Type

case class TRecord(fields: Vector[(String, Type)]) extends Type {
  override def toString: String = "R{" + fields.foldLeft("")((s, p) => s + p._1 + ": " + p._2 + ", ") + "}"
}
