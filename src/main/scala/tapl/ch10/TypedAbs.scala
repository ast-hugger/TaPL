package tapl.ch10

import tapl.base.ExtendedLambdaCalculus.{Term, Var, traits}
import tapl.base.{ExtendedLambdaCalculus, Type}

/**
  * Because there is no type inference, instead of [[ExtendedLambdaCalculus.Abs]]
  * we are using this.
  */
case class TypedAbs(variable: Var, varType: Type, body: Term) extends Term with traits.Abs {
  override def toString: String = s"\u03BB$variable: $varType. $body"
  override def replicateUsing(mapping: ExtendedLambdaCalculus.traits.Mapping): Term = {
    val varCopy = variable.replicateUsing()
    TypedAbs(varCopy, varType, body.rewriteUsing(mapping + (variable -> varCopy)))
  }
}

object TypedAbs {
  def lambda(name: String, varType: Type)(bodyGen: Var => Term): TypedAbs = {
    val v = Var(name)
    TypedAbs(v, varType, bodyGen(v))
  }
}
