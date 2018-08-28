package tapl.ch22

import tapl.base.Type

/**
  * A type variable.
  */
case class TVar(name: String) extends Type {
  override def toString: String = "T" + name
}

object TVar {
  private var serial = 0
  def generate(): TVar = {
    serial += 1
    new TVar("G" + serial)
  }
}
