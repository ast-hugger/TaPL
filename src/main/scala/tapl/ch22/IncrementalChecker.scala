package tapl.ch22

import tapl.base.ExtendedLambdaCalculus._
import tapl.base.{TBool, _}
import tapl.ch10.TypedAbs

import scala.util.{Failure, Success, Try}

/**
  * Type reconstructing checker incrementally unifying the generated constraints
  * (Exercise 22.5.7). The core structure is along the lines of Hindley-Milner
  * algorithm W (I guess), so that the type inference function returns the type
  * of the term together with variable substitutions performed to produce it,
  * and which must be applied to all types "upstream" of the invocation.
  */
object IncrementalChecker {

  type Context = Map[String, Type]
  type VarSubst = Map[TVar, Type]

  def typeOf(term: Term, context: Context): (Type, Substitution) = term match {
    case Var(name) => (context(name), Substitution.empty)
    case _: BoolLiteral => (TBool, Substitution.empty)
    case _: IntLiteral => (TInt, Substitution.empty)
    case Succ(expr) => {
      val (eType, eSub) = typeOf(expr, context)
      (TInt, eSub composedWith unify(eType, TInt))
    }
    case IsZero(expr) => {
      val (eType, eSub) = typeOf(expr, context)
      (TBool, eSub composedWith unify(eType, TInt))
    }
    case TypedAbs(Var(varName), varType, body) => {
      val bodyContext = context + (varName -> varType)
      val (bType, bSub) = typeOf(body, bodyContext)
      (TFun(bSub(varType), bType), bSub)
    }
    case App(fun, arg) => {
      val resultType = TVar.generate()
      val (ftype, fsub) = typeOf(fun, context)
      val (atype, asub) = typeOf(arg, context)
      val u = unify(ftype, TFun(atype, resultType))
      val combined = asub composedWith (fsub composedWith u)
      (combined(resultType), combined)
    }
    case If(guard, trueBranch, falseBranch) => {
      val (gtype, gsub) = typeOf(guard, context)
      val (ttype, tsub) = typeOf(trueBranch, context)
      val (ftype, fsub) = typeOf(falseBranch, context)
      val u1 = unify(ttype, ftype)
      val u2 = unify(gtype, TBool)
      val combined = u1 composedWith (u2 composedWith (gsub composedWith (tsub composedWith fsub)))
      (combined(ttype), combined)
    }
    case _ => throw new InferenceError(s"Inference not implemented for term: $term")
  }

  private def unify(t: Type, s: Type): Substitution = (t, s) match {
    case (TBool, TBool) => Substitution.empty
    case (TInt, TInt) => Substitution.empty
    case (x, y) if x == y => Substitution.empty
    case (v: TVar, t) => Substitution(v, t)
    case (t, v: TVar) => Substitution(v, t)
    case (TFun(f1, t1), TFun(f2, t2)) => unify(f1, f2) composedWith unify(t1, t2)
    case _ => throw new InferenceError(s"can't unify $t and $s")
  }

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
      case Success((t, sub)) =>
        println(s"\ttype: $t")
        println(s"\tsubstitution: $sub")
      case Failure(ex) => println(s"\terror: ${ex.getMessage}")
    }
  }

  def trace[T](x: T): T = {
    println("TRACING: " + x)
    x
  }

  class InferenceError(message: String) extends Exception(message)
}
