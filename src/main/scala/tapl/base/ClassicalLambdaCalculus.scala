package tapl.base

object ClassicalLambdaCalculus {
  object Traits extends Terms { type T = Term }

  sealed abstract class Term extends Traits.Rewritable {
    def apply(arg: Term): Term = App(this, arg)
  }

  case class Var(name: String) extends Term with Traits.Var {
    override def replicateUsing(mapping: Traits.Mapping = Map()): Var = Var(name)
  }

  case class App(fun: Term, arg: Term) extends Term with Traits.App {
    override def replicateUsing(mapping: Traits.Mapping): App =
      App(fun.rewriteUsing(mapping), arg.rewriteUsing(mapping))
  }

  case class Abs(variable: Var, body: Term) extends Term with Traits.Abs {
    override def replicateUsing(mapping: Traits.Mapping): Abs = {
      val varCopy = variable.replicateUsing()
      Abs(varCopy, body.rewriteUsing(mapping + (variable -> varCopy)))
    }
  }

  def lambda(name: String)(body: Var => Term): Term = {
    val v = Var(name)
    Abs(v, body(v))
  }

  def eval1(term: Term): Option[Term] = term match {
    case App(Abs(v, body), arg) if arg.isValue => Some(body.rewriteUsing(Map(v -> arg)))
    case App(f, arg) if f.isValue => eval1(arg).map(App(f, _))
    case App(f, arg) => eval1(f).map(App(_, arg))
    case _ => None
  }

  /** Same as [[eval1]] but include into the result the substituted variable and term.  */
  def eval1withSubst(term: Term): Option[(Term, Var, Term)] = term match {
    case App(Abs(v, body), arg) if arg.isValue => Some((body.rewriteUsing(Map(v -> arg)), v, arg))
    case App(f, arg) if f.isValue => eval1withSubst(arg).map { case (r, v, s) => (App(f, r), v, s) }
    case App(f, arg) => eval1withSubst(f).map { case (r, v, s) => (App(r, arg), v, s) }
    case _ => None
  }
}
