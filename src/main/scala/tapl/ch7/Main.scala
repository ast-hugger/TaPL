package tapl.ch7

import tapl.base.ClassicLambdaCalculus
import tapl.base.ClassicLambdaCalculus._

object Main extends App {

  val lambda: String => (Variable => Term) => Term = ClassicLambdaCalculus.lambda

  /*
    Church booleans and naturals:

    true = λt. λf. t
    false = λt. λf. f

    not = λv. v f t
    and = λv. λw. v w f
    or = λv. λw. v t w

    zero = λs. λz. z
    succ = λn. λs. λz. s (n s z)
    iszero = λn. n (λx. cfalse) ctrue
   */

  val ctrue = lambda("t") { t => lambda("f") { f => t } }
  val cfalse = lambda("t") { t => lambda("f") { f => f } }

  val cnot = lambda("v") { v => v(cfalse)(ctrue) }
  val cand = lambda("v") { v => lambda("w") { w => v(w)(cfalse) } }
  val cor = lambda("v") { v => lambda("w") { w => v(ctrue)(w) } }

  val czero = lambda("s") { s => lambda("z") { z => z } }
  val csucc = lambda("n") { n => lambda("s") { s => lambda("z") { z => s(z) } } }
  val ciszero = lambda("n") { n => n(lambda("x") { x => cfalse })(ctrue) }

  display("false", cfalse)
  display("not(true)", cnot(ctrue))
  display("not(false)", cnot(cfalse))

  display("and(true, true)", cand(ctrue)(ctrue))
  display("and(true, false)", cand(ctrue)(cfalse))
  display("and(false, true)", cand(cfalse)(ctrue))
  display("and(false, false)", cand(cfalse)(cfalse))

  display("or(true, true)", cor(ctrue)(ctrue))
  display("or(true, false)", cor(ctrue)(cfalse))
  display("or(false, true)", cor(cfalse)(ctrue))
  display("or(false, false)", cor(cfalse)(cfalse))

  display("zero", czero)
  display("succ(zero)", csucc(czero))
  display("iszero(zero)", ciszero(czero))
  display("iszero(succ(zero))", ciszero(csucc(czero)))

  private def display(label: String, term: Term): Unit = {
    println(label)
    printReductions(term)
  }

  private def printReductions(term: Term): Unit = {
    println(term)
    ClassicLambdaCalculus.eval1(term) match {
      case Some(result) => {
        println(s"    => $result")
        printReductions(result)
      }
      case _ => println()
    }
  }
}
