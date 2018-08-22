package tapl.base

trait Variable extends Term {
  type Self <: Variable

  val name: String

  override def freeVariables: Set[Variable] = Set(this)

  override private[base] def copyUsing(varMapping: Map[Variable, Variable]): this.type =
    varMapping.getOrElse(this, this).asInstanceOf[this.type]

  def replicate(): Self

  override def substitute(variable: Variable, replacement: Term): Term =
    if (variable == this) replacement.copy else this

  override def toString: String = name
}

