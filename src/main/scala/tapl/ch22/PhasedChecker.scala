package tapl.ch22

import tapl.base.ExtendedLambdaCalculus._
import tapl.base._
import tapl.ch10.TypedAbs

/**
  * Type reconstructing checker with separate constraint generation and
  * unification phases (Exercise 22.5.5).
  */
object PhasedChecker {

  type Context = Map[String, Type]
  type Constraint = (Type, Type)
  type Substitution = (TVar, Type)

  /**
    * Figure 22-1
    */
  def typeAndConstraints(term: Term, context: Context): (Type, List[Constraint]) = term match {
    case Var(name) => (context(name), Nil)
    case _: BoolLiteral => (TBool, Nil)
    case _: IntLiteral => (TInt, Nil)
    case Succ(expr) =>
      val (etype, econstr) = typeAndConstraints(expr, context)
      (TInt, (etype, TInt) :: econstr)
    case IsZero(expr) =>
      val (etype, econstr) = typeAndConstraints(expr, context)
      (TBool, (etype, TInt) :: econstr)
    case TypedAbs(Var(varName), varType, body) =>
      val (bodyType, constr) = typeAndConstraints(body, context + (varName -> varType))
      (TFun(varType, bodyType), constr)
    case App(fun, arg) =>
      val (fType, fConstr) = typeAndConstraints(fun, context)
      val (aType, aConstr) = typeAndConstraints(arg, context)
      val resultVar = TVar.generate()
      (resultVar, (fType, TFun(aType, resultVar)) :: (fConstr ++ aConstr))
    case If(guard, trueBranch, falseBranch) =>
      val (gt, gc) = typeAndConstraints(guard, context)
      val (tt, tc) = typeAndConstraints(trueBranch, context)
      val (ft, fc) = typeAndConstraints(falseBranch, context)
      (tt, (gt, TBool) :: (tt, ft) :: gc ++ tc ++ fc)
  }

  def unify(cs: List[Constraint]): Either[String, List[Substitution]] = cs match {
    case Nil => Right(Nil)
    case (v @ TVar(_), t) :: rest if !(freeVars(t) contains v) => unify(subst(rest, v, t)).map((v -> t) :: _)
    case (t, v @ TVar(_)) :: rest if !(freeVars(t) contains v) => unify(subst(rest, v, t)).map((v -> t) :: _)
    case (t1, t2) :: rest if t1 == t2 => unify(rest)
    case (t, s) :: _ => Left("Can't unify $t and $s")
  }

  private def subst(t: Type, v: TVar, repl: Type): Type = t match {
    case TFun(t1, t2) => TFun(subst(t1, v, repl), subst(t2, v, repl))
    case tv : TVar if tv == v => repl
    case _ => t
  }

  private def subst(cs: List[Constraint], v: TVar, repl: Type): List[Constraint] =
    cs map { case (t1, t2) => (subst(t1, v, repl), subst(t2, v, repl)) }

  private def freeVars(t: Type): Set[TVar] = t match {
    case tv: TVar => Set(tv)
    case TFun(from, to) => freeVars(from) & freeVars(to)
    case _ => Set.empty
  }

  private def applySubst(t: Type, ss: List[Substitution]) = ss.foldLeft(t)((t, s) => subst(t, s._1, s._2))

  /**
    * Create a typed abstraction from typeless syntax,
    * with a fresh type variable as the type.
    */
  private def lambda(varName: String)(bodyGen: Var => Term): Term = TypedAbs.lambda(varName, TVar.generate())(bodyGen)

  def main(args: Array[String]): Unit = {
    printTypeAndConstraintsOf(lambda("a") { a => a })
    printTypeAndConstraintsOf(lambda("a") { a => Succ(a) })
    printTypeAndConstraintsOf(lambda("a") { a => IsZero(a) })
    printTypeAndConstraintsOf(lambda("b") { b => lambda("n") { n => If(b, n, Succ(n)) } })
  }

  private def printTypeAndConstraintsOf(expr: Term): Unit = {
    println(expr)
    val (exprType, exprConstr) = typeAndConstraints(expr, Map())
    val unification = unify(exprConstr)
    val principal = unification.map(applySubst(exprType, _)).map(_.toString) getOrElse "?"
    println(s"\traw type: $exprType\n\tconstraints: $exprConstr\n\tunification: $unification\n\tprincipal type: $principal")
  }
}
