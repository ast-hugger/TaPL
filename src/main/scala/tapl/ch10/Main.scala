package tapl.ch10

import tapl.base.{ExtendedLambdaCalculus, _}
import tapl.base.ExtendedLambdaCalculus._

/**
  * Simply typed extended lambda calculus
  */
object Main {

  /**
    * Because there is no type inference, instead of [[ExtendedLambdaCalculus.Abs]]
    * we are using this.
    */
  case class TypedAbs(variable: Var, varType: Type, body: Term) extends Term with traits.Abs {
    override def toString: String = s"\u03BB$variable: $varType. $body"
    override def replicateUsing(mapping: ExtendedLambdaCalculus.traits.Mapping): Term = {
      val varCopy = variable.replicateUsing()
      TypedAbs(varCopy, varType, body.rewriteUsing(mapping + (variable -> varCopy)))
    }
  }

  def lambda(name: String, varType: Type)(bodyGen: Var => Term): TypedAbs = {
    val v = Var(name)
    TypedAbs(v, varType, bodyGen(v))
  }

  type Context = Map[String, Type]

  def typeOf(term: Term, context: Context = Map()): Either[String, Type] = term match {
    case _: BoolLiteral => Right(TBool)
    case _: IntLiteral => Right(TInt)
    case Var(name) => (context get name).toRight(s"variable $name is undefined")
    case App(fun, arg) => for {
        argType <- typeOf(arg, context)
        funType <- typeOf(fun, context)
          .filterOrElse(_.isInstanceOf[TFun], s"$fun is not a function")
          .map(_.asInstanceOf[TFun])
          .filterOrElse(_.from == argType, s"function does not accept $argType")
      } yield funType.to
    case TypedAbs(Var(name), vType, body) =>
      typeOf(body, context + (name -> vType)).map(TFun(vType, _))
    case If(guard, trueBranch, falseBranch) => for {
      guardType <- typeOf(guard, context).filterOrElse(_ == TBool, "non-boolean if guard")
      trueType <- typeOf(trueBranch, context)
      falseType <- typeOf(falseBranch, context).filterOrElse(_ == trueType, "if branches have different types")
    } yield trueType
    case Let(Var(name), init, body) => for {
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
    printTypeOf(lambda("b", TBool) { b => tif(b, int(1), int(0)) } (ttrue))
    printTypeOf(lambda("b", TBool) { b => tif(b, int(1), int(0)) } (int(42)))
    printTypeOf(let("a", int(42)) { a => lambda("x", TBool) { x => a }})
  }
}

