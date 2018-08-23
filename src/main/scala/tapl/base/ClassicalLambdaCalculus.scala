package tapl.base

object ClassicalLambdaCalculus {
  object Traits extends Terms[Term]

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
}
