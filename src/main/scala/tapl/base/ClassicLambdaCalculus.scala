package tapl.base

import tapl.base

object ClassicLambdaCalculus {

  sealed abstract class Term extends base.Term {
    def apply(arg: Term): Term = Application(this, arg)
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

  case class Abstraction(variable: Variable, body: Term) extends Term with base.Abstraction {
    type Self = Abstraction
    type V = Variable
    type B = Term

    override protected def construct(v: Variable, b: Term): Abstraction = Abstraction(v, b)
  }

  case class Application(fun: Term, arg: Term) extends Term with base.Application {
    type Self = Application
    type T = Term

    override protected def construct(f: Term, a: Term): Application = Application(f, a)
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
    case _ => None
  }

  def eval(term: Term): Term = eval1(term).map(eval) getOrElse term
}
