package tapl.base

trait Literal[T] extends Term {
  type Self <: Literal[T]
  val value: T

  protected def replicate(): Self

  override def isValue: Boolean = true

  override def freeVariables: Set[Variable] = Set()

  override private[base] def copyUsing(varMapping: Map[Variable, Variable]): this.type =
    replicate().asInstanceOf[this.type]

  override def substitute(variable: Variable, replacement: Term): Term = this
}
