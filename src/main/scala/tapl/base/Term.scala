package tapl.base

trait Term {
  def isValue: Boolean = false
  def freeVariables: Set[Variable]
  def copy: this.type = copyUsing(Map())
  private[base] def copyUsing(varMapping: Map[Variable, Variable]): this.type
  def substitute(variable: Variable, replacement: Term): Term
}
