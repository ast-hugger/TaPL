package tapl.base

trait Abstraction extends Term {
  type Self <: Abstraction
  type V <: Variable
  type B <: Term

  val variable: V
  val body: B

  protected def construct(v: V, b: B): Self

  override def isValue: Boolean = true

  override def freeVariables: Set[Variable] = body.freeVariables - variable

  override private[base] def copyUsing(varMapping: Map[Variable, Variable]): this.type = {
    val newVar = variable.replicate().asInstanceOf[V]
    construct(newVar, body.copyUsing(varMapping + (variable -> newVar))).asInstanceOf[this.type]
  }

  override def substitute(variable: Variable, replacement: Term): Term =
    construct(this.variable, body.substitute(variable, replacement).asInstanceOf[B])

  override def toString: String = s"\u03BB$variable. $body"
}
