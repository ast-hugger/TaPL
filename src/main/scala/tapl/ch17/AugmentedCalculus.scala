package tapl.ch17

import tapl.base.ExtendedLambdaCalculus
import tapl.base.ExtendedLambdaCalculus.{Abs, Term, Var, traits}
import tapl.base.Util._

/**
  * Extended lambda calculus from the base further augmented with records.
  */
object AugmentedCalculus {
  private val base = ExtendedLambdaCalculus

  case class Record(fields: Vector[(String, Term)]) extends Term {
    override def replicateUsing(mapping: traits.Mapping): Term =
      Record(fields.map { case (s, t) => (s, t.rewriteUsing(mapping))} )
    override def toString: String = "{" + mapAndJoin(fields)(p => p._1 + " -> " + p._2) { _ + ", " + _ } + "}"
  }

  case class Project(record: Term, field: String) extends Term {
    override def replicateUsing(mapping: traits.Mapping): Term = Project(record.rewriteUsing(mapping), field)
    override def toString: String = s"$record.$field"
  }

  def lambda(name: String)(body: Var => Term): Abs = {
    val v = Var(name)
    Abs(v, body(v))
  }

  def record(iterable: Iterable[(String, Term)]): Record = Record(iterable.toVector)

  def eval1(term: Term): Option[Term] = term match {
    case Project(Record(fields), field) => fields.find(_._1 == field).map(_._2)
    case Project(x, f) if !x.isValue => eval1(x).map(Project(_, f))
    case _ => base.eval1(term)
  }
}
