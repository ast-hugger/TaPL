package tapl.ch22

import tapl.base.Type

/**
  * A type variable.
  */
case class TVar(name: String) extends Type {
  override def toString: String = name
}

object TVar {
  private var serial = 0
  private val letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" // ought to be enough for everyone
  def generate(): TVar = {
    serial += 1
    new TVar(letters(serial).toString)
  }
}
