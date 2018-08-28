package tapl.ch22

import tapl.base.ExtendedLambdaCalculus.{Term, traits}

case class IsZero(expr: Term) extends Term {
  override def replicateUsing(mapping: traits.Mapping): Term = IsZero(expr.rewriteUsing(mapping))
}
