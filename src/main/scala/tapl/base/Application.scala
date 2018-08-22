package tapl.base

trait Application extends Term {
  type Self <: Application
  type T <: Term

  val fun: T
  val arg: T

  protected def construct(fun: T, arg: T): Self

  override def freeVariables: Set[Variable] = fun.freeVariables union arg.freeVariables

  override private[base] def copyUsing(varMapping: Map[Variable, Variable]): this.type =
    construct(fun.copyUsing(varMapping), arg.copyUsing(varMapping)).asInstanceOf[this.type]

  override def substitute(variable: Variable, replacement: Term): Term =
    construct(
      fun.substitute(variable, replacement).asInstanceOf[T],
      arg.substitute(variable, replacement).asInstanceOf[T])

  override def toString: String = fun match {
    case _: Abstraction => s"($fun) $arg"
    case _: Application => s"($fun) $arg"
    case _ => s"$fun $arg"
  }
}
