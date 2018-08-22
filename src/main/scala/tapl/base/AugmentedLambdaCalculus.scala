package tapl.base

import tapl.base

/**
  * The lambda calculus augmented with booleans, integers, `if`, and `let`.
  */
object AugmentedLambdaCalculus {

  sealed abstract class Term extends base.Term {
    def apply(arg: Term): Term = Application(this, arg)
  }

  case class IntLiteral(value: Int) extends Term with base.Literal[Int] {
    override type Self = IntLiteral
    override protected def replicate(): IntLiteral = this
  }

  case class BoolLiteral(value: Boolean) extends Term with base.Literal[Boolean] {
    override type Self = BoolLiteral
    override protected def replicate(): BoolLiteral = this
  }

  case class Variable(name: String) extends Term with base.Variable {
    type Self = Variable

    override def replicate(): Variable = Variable(name)

    /**
      * Variable equality in our model is identity-based;
      * case class default [[equals]] is not what we want.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case otherVar: Variable => this eq otherVar
      case _ => super.equals(obj)
    }
  }

  case class If(guard: Term, trueBranch: Term, falseBranch: Term) extends Term {
    override def freeVariables: Set[base.Variable] =
      guard.freeVariables union trueBranch.freeVariables union falseBranch.freeVariables
    override private[base] def copyUsing(varMapping: Map[base.Variable, base.Variable]): this.type =
      If(guard.copyUsing(varMapping), trueBranch.copyUsing(varMapping), falseBranch.copyUsing(varMapping))
      .asInstanceOf[this.type]
    override def substitute(variable: base.Variable, replacement: base.Term): base.Term =
      If(guard.substitute(variable, replacement).asInstanceOf[Term],
        trueBranch.substitute(variable, replacement).asInstanceOf[Term],
        falseBranch.substitute(variable, replacement).asInstanceOf[Term])
  }

  case class Let(variable: Variable, init: Term, body: Term) extends Term {
    override def freeVariables: Set[base.Variable] =
      init.freeVariables union (body.freeVariables - variable)

    override private[base] def copyUsing(varMapping: Map[base.Variable, base.Variable]): this.type = {
      val newVar = variable.replicate()
      val augmentedMapping = varMapping + (variable -> newVar)
      Let(newVar, init.copyUsing(varMapping), body.copyUsing(augmentedMapping)).asInstanceOf[this.type]
    }

    override def substitute(variable: base.Variable, replacement: base.Term): base.Term =
      Let(this.variable,
        init.substitute(variable, replacement).asInstanceOf[Term],
        body.substitute(variable, replacement).asInstanceOf[Term])
  }

  case class Application(fun: Term, arg: Term) extends Term with base.Application {
    type Self = Application
    type T = Term

    override protected def construct(f: Term, a: Term): Application = Application(f, a)
  }

  case class Abstraction(variable: Variable, body: Term) extends Term with base.Abstraction {
    type Self = Abstraction
    type V = Variable
    type B = Term

    override protected def construct(v: Variable, b: Term): Abstraction = Abstraction(v, b)
  }

  def lambda(name: String)(body: Variable => Term): Abstraction = {
    val v = Variable(name)
    Abstraction(v, body(v))
  }

  def eval1(term: Term): Option[Term] = term match {
    case Application(Abstraction(variable, body), arg) if arg.isValue => {
      println(s"    [$variable -> $arg] $body")
      Some(body.substitute(variable, arg).asInstanceOf[Term])
    }
    case Application(fun, arg) if fun.isValue => eval1(arg).map(Application(fun, _))
    case Application(fun, arg) => eval1(fun).map(Application(_, arg))
    case Let(variable, init, body) if init.isValue => {
      println(s"    [$variable -> $init] $body")
      Some(body.substitute(variable, init).asInstanceOf[Term])
    }
    case Let(variable, init, body) => eval1(init).map(Let(variable, _, body))
    case If(BoolLiteral(true), trueBranch, falseBranch) => eval1(trueBranch)
    case If(BoolLiteral(false), trueBranch, falseBranch) => eval1(falseBranch)
    case If(guard, trueBranch, falseBranch) => eval1(guard).map(If(_, trueBranch, falseBranch))
    case _ => None
  }

  def eval(term: Term): Term = eval1(term).map(eval) getOrElse term
}
