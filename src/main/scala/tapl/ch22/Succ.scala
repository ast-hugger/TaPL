package tapl.ch22

import tapl.base.ExtendedLambdaCalculus.{Term, traits}

case class Succ(expr: Term) extends Term {
  override def replicateUsing(mapping: traits.Mapping): Term = Succ(expr.rewriteUsing(mapping))
}
