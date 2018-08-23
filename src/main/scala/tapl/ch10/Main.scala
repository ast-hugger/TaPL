package tapl.ch10

import tapl.base._
import tapl.base.ExtendedLambdaCalculus._
import TypedAbs.lambda

/**
  * Simply typed extended lambda calculus
  */
object Main {
  type Context = Map[String, Type]

  def typeOf(term: Term, context: Context = Map()): Either[String, Type] = term match {
    case _: BoolLiteral => Right(TBool)
    case _: IntLiteral => Right(TInt)
    case Var(name) => (context get name).toRight(s"variable $name is undefined")
    case App(fun, arg) =>
      for {
        argType <- typeOf(arg, context)
        leftType <- typeOf(fun, context)
        funType <- leftType match {
          case f: TFun => Right(f)
          case t @ _ => Left(s"not a function: $t")
        }
        t <- if (funType.from == argType) Right(funType) else Left(s"function expects a ${funType.from}")
      } yield t.to
    case TypedAbs(Var(name), vType, body) =>
      typeOf(body, context + (name -> vType)).map(TFun(vType, _))
    case If(guard, trueBranch, falseBranch) =>
      for {
        _ <- typeOf(guard, context).filterOrElse(_ == TBool, "non-boolean if guard")
        trueType <- typeOf(trueBranch, context)
        falseType <- typeOf(falseBranch, context)
        commonType <- if (trueType == falseType) Right(trueType) else Left("branches have different types")
      } yield commonType
    case Let(Var(name), init, body) =>
      for {
        varType <- typeOf(init, context)
        bodyType <- typeOf(body, context + (name -> varType))
      } yield bodyType
    case _ => Left(s"unrecognized term $term")
  }

  def printTypeOf(term: Term): Unit = {
    print(s"$term: ")
    typeOf(term) match {
      case Right(t) => println(t)
      case Left(err) => println(s"error: $err")
    }
  }

  val ttrue = BoolLiteral(true)
  def int(v: Int) = IntLiteral(v)
  def tif(guard: Term, tb: Term, fb: Term) = If(guard, tb, fb)
  def let(varName: String, init: Term)(bodyGen: Var => Term): Let = {
    val v = Var(varName)
    Let(v, init, bodyGen(v))
  }

  def main(args: Array[String]): Unit = {
    printTypeOf(ttrue)
    printTypeOf(int(42))
    printTypeOf(lambda("b", TBool) { b => b })
    printTypeOf(lambda("b", TBool) { b => tif(b, int(1), int(0)) })
    printTypeOf(lambda("b", TInt) { b => tif(b, int(1), int(0)) })
    printTypeOf(lambda("b", TBool) { b => tif(b, ttrue, int(0)) })
    printTypeOf(lambda("b", TBool) { b => tif(b, int(1), int(0)) } (ttrue))
    printTypeOf(lambda("b", TBool) { b => tif(b, int(1), int(0)) } (int(42)))
    printTypeOf(let("a", int(42)) { a => lambda("x", TBool) { x => a }})
  }
}

