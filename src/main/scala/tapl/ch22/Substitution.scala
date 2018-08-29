package tapl.ch22

import tapl.base.{TFun, Type}

/**
  * A set of type variable substitutions.
  */
class Substitution private (val map: Map[TVar, Type] ) {

  def + (binding: (TVar, Type)): Substitution = {
    val (tvar, t) = binding
    map.get(tvar) match {
      case Some(t2) =>
        if (t == t2) this
        else throw new IncrementalChecker.InferenceError(s"$tvar binding conflict: $t2 and $t")
      case None =>
        val rewriter = Substitution(tvar, t)
        val augmented = map + binding
        augmented.mapValues(v => rewriter(v))
        new Substitution(augmented)
    }
  }

  def + (other: Substitution): Substitution = map.foldLeft(other) { (subst, binding) => subst + binding }

  def unify(t: Type, s: Type): Substitution = t match {
    case tv: TVar => this + (tv -> s)
    case _ => this
  }

  def apply(t: Type): Type = t match {
    case v: TVar => map.getOrElse(v, t)
    case TFun(from, to) => TFun(this(from), this(to))
    case _ => t
  }
}

object Substitution {
  val empty = new Substitution(Map.empty)

  def apply(v: TVar, t: Type) = new Substitution(Map(v -> t))
}