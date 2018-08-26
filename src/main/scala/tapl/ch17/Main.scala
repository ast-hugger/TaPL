package tapl.ch17

import tapl.base.ExtendedLambdaCalculus._
import tapl.base._
import tapl.ch10.TypedAbs
import tapl.ch17.AugmentedCalculus._

/**
  * Simply typed lambda calculus with subtyping.
  */
object Main {
  type Context = Map[String, Type]

  def isSubtype(a: Type, b: Type): Boolean = (a, b) match {
    case (TBottom, _) => true
    case (_, TTop) => true
    case (TRecord(af), TRecord(bf)) =>
      bf.forall { case (bn, bt) => af.exists { case (an, at) => an == bn && isSubtype(at, bt) }}
    case (TFun(f1, t1), TFun(f2, t2)) => isSubtype(f2, f1) && isSubtype(t1, t2)
    case _ => a == b
  }

  def typeOf(term: Term, context: Context = Map()): Either[String, Type] = term match {
    case BoolLiteral(_) => Right(TBool)

    case IntLiteral(_) => Right(TInt)

    case Var(name) => (context get name).toRight(s"variable $name is undefined")

    case Record(fields) =>
      val fieldNames = fields.map(_._1)
      val fieldTypes = fields.map(p => typeOf(p._2, context))
      Util.allRightsOrLeft(fieldTypes).map(fieldNames zip _).map(TRecord)

    case Project(record, field) =>
      typeOf(record, context) match {
        case Right(TRecord(fieldTypes)) =>
          fieldTypes.find(_._1 == field).map(p => Right(p._2)) getOrElse Left(s"field $field not found")
        case Right(t) => Left(s"projection expected a record type, got $t")
        case e @ Left(_) => e
      }

    case TypedAbs(variable, varType, body) =>
      val augmented = context + (variable.name -> varType)
      typeOf(body, augmented).map(TFun(varType, _))

    case App(fun, arg) =>
      typeOf(fun, context) match {
        case Right(TFun(from, to)) =>
          typeOf(arg, context) match {
            case Right(argType) if isSubtype(argType, from) => Right(to)
            case Right(t) => Left(s"wrong argument type: $t")
            case x => x
          }
        case Right(t) => Left("function type expected, got $t")
        case x => x
      }

    case _ => Left(s"unrecognized term $term")
  }

  def printTypeOf(term: Term): Unit = {
    print(s"$term: ")
    typeOf(term) match {
      case Right(t) => println(t)
      case Left(err) => println(s"error -- $err")
    }
  }

  val a = Record(Vector(
    "one" -> IntLiteral(2)))

  val aType = TRecord(Vector("one" -> TInt))

  val b = Record(Vector(
    "two" -> BoolLiteral(false)))

  val c = Record(Vector( // type(c) is a subtype of both type(a) and type(b)
    "one" -> IntLiteral(1),
    "two" -> BoolLiteral(true)))

  private val anyFun = TypedAbs.lambda("x", TTop) { x => x }
  private val intFun = TypedAbs.lambda("n", TInt) { x => x }
  private val aFun = TypedAbs.lambda("r", aType) { x => Project(x, "one") }
  private val uncallableFun = TypedAbs.lambda("a", TBottom) { x => IntLiteral(42) }

  def main(args: Array[String]): Unit = {
    printTypeOf(a)
    printTypeOf(b)
    printTypeOf(c)
    printTypeOf(anyFun)
    printTypeOf(intFun)
    printTypeOf(aFun)
    printTypeOf(uncallableFun)
    header("projections")
    printTypeOf(Project(c, "one"))
    printTypeOf(Project(c, "two"))
    header("applications")
    printTypeOf(anyFun(a))
    printTypeOf(anyFun(Project(a, "one")))
    printTypeOf(intFun(Project(a, "one")))
    printTypeOf(aFun(a))
    printTypeOf(aFun(c))
    header("error cases")
    printTypeOf(aFun(b))
    printTypeOf(Project(a, "bogus"))
    printTypeOf(intFun(Project(a, "two")))
    printTypeOf(uncallableFun(a))
  }

  private def header(text: String): Unit = {
    println("  " + text)
  }
}
