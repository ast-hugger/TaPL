package tapl.ch22

import tapl.base.ExtendedLambdaCalculus._
import tapl.base._
import tapl.ch10.TypedAbs

import scala.util.{Failure, Success, Try}

/** Type reconstructing checker incrementally unifying the generated constraints
 * (Exercise 22.5.7). The core structure is along the lines of Hindley-Milner
 * algorithm W, so that the type inference function returns the type of the
 * term together with variable substitutions performed to produce it, and which
 * must be applied to all types "upstream" of the invocation.
  */
object IncrementalChecker {

  type Context = Map[String, Type]
  type VarSubst = Map[TVar, Type]

  def typeOf(term: Term, context: Context): (Type, Substitution) = term match {
    case Var(name) => (context(name), Substitution.empty)
    case _: BoolLiteral => (TBool, Substitution.empty)
    case _: IntLiteral => (TInt, Substitution.empty)
    case Succ(expr) =>
      val (t, sub) = typeOf(expr, context)
      (TInt, sub.unify(t, TInt))
    case IsZero(expr) =>
      val (t, sub) = typeOf(expr, context)
      (TInt, sub.unify(t, TBool))
    case TypedAbs(Var(varName), varType, body) =>
      val bodyContext = context + (varName -> varType)
      val (t, sub) = typeOf(body, bodyContext)
      (TFun(sub(varType), t), sub)
    // DEFINITELY NOT CORRECT PAST THIS POINT
    case App(fun, arg) => // bogus, just messing around
      val (atype, asub) = typeOf(arg, context)
      val (ftype, fsub) = typeOf(fun, context)
      val tv = TVar.generate()
      val aug = (asub + fsub).unify(ftype, TFun(atype, tv))
      (aug(tv), aug)
    case If(guard, trueBranch, falseBranch) =>
      val (gtype, gsub) = typeOf(guard, context)
      val (ttype, tsub) = typeOf(trueBranch, context)
      val (ftype, fsub) = typeOf(falseBranch, context)
      val gType = TVar.generate()
      val rType = TVar.generate()
      val combined = gsub + tsub + fsub + (gType -> TBool) + (rType -> ttype) + (rType -> ftype)
      (combined(rType), combined)
    case _ => throw new InferenceError(s"Inference not implemented for term: $term")
  }

  private def add(substs: VarSubst, t1: Type, t2: Type): Either[String, VarSubst] = t1 match {
    case tv: TVar =>
      if (substs contains tv) {
        if (substs(tv) == t2) Right(substs)
        else Left(s"conflicting inferred types for $tv")
      } else {
        Right(substs + (tv -> t2))
      }
    case _ if t1 == t2 => Right(substs)
    case _ => Left(s"types $t1 and $t2 are not unifiable")
  }

  private def apply(subst: VarSubst, t: Type): Type = t match {
    case tv: TVar => subst.getOrElse(tv, t)
    case TFun(from, to) => TFun(apply(subst, from), apply(subst, to))
    case _ => t
  }

  private def union(s1: VarSubst, s2: VarSubst): Either[String, VarSubst] = ???

  /**
    * Create a typed abstraction from typeless syntax, with a fresh type variable as the type.
    */
  private def lambda(varName: String)(bodyGen: Var => Term): Term = TypedAbs.lambda(varName, TVar.generate())(bodyGen)

  def main(args: Array[String]): Unit = {
    printTypeOf(lambda("a") { a => a })
    printTypeOf(lambda("a") { a => Succ(a) })
    printTypeOf(lambda("a") { a => IsZero(a) })
    printTypeOf(lambda("b") { b => lambda("n") { n => If(b, n, Succ(n)) } })
    printTypeOf(lambda("x") { x => lambda("y") { y =>  lambda("z") { z => x(z)(y(z)) } } }) // exercise 22.5.2
    printTypeOf(lambda("z") { z => lambda("y") { y => z(y(BoolLiteral(true))) }})
    printTypeOf(lambda("w") { w => If(BoolLiteral(true), BoolLiteral(false), w(BoolLiteral(false)))})
  }

  private def printTypeOf(expr: Term): Unit = {
    println(expr)
    Try { typeOf(expr, Map()) } match {
      case Success((t, sub)) => println(s"\tprincipal type: $t")
      case Failure(ex) => println(s"\terror: ${ex.getMessage}")
    }
  }

  class InferenceError(message: String) extends Exception(message)
}
