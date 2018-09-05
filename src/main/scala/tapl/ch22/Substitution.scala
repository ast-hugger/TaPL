package tapl.ch22

import tapl.base.{TFun, Type}

/**
  * A mapping from type variables to types.
  */
class Substitution private (val map: Map[TVar, Type] ) {

  def apply(t: Type): Type = t match {
    case v: TVar => map.getOrElse(v, t)
    case TFun(from, to) => TFun(this(from), this(to))
    case _ => t
  }

  def rangeContains(variable: TVar): Boolean = map contains variable

  def composedWith(another: Substitution): Substitution = {
    val applied = another.map map { case (v, t) => (v, this(t)) }
    val merged = applied ++ (map filter { case (v, _) => !another.rangeContains(v) })
    new Substitution(merged)
  }

  override def toString: String = "{" + map.map { case (v, t) => v + " = " + t }.mkString(", ") + "}"
}

object Substitution {
  val empty = new Substitution(Map.empty)

  /**
    * Create a new instance mapping the specified variable to the specified type.
    */
  def apply(v: TVar, t: Type) = new Substitution(Map(v -> t))
}