package tapl.ch7

import tapl.base.ClassicalLambdaCalculus
import tapl.base.ClassicalLambdaCalculus.{Var, Term, eval1withSubst}

object Main extends App {

  //noinspection ScalaUnnecessaryParentheses -- parentheses are necessary here
  val lambda: String => (Var => Term) => Term = ClassicalLambdaCalculus.lambda

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

  def ctrue: Term = lambda("t") { t => lambda("f") { f => t } }.replicate
  def cfalse: Term = lambda("t") { t => lambda("f") { f => f } }.replicate

  def cnot: Term = lambda("v") { v => v(cfalse)(ctrue) }.replicate
  def cand: Term = lambda("v") { v => lambda("w") { w => v(w)(cfalse) } }.replicate
  def cor: Term = lambda("v") { v => lambda("w") { w => v(ctrue)(w) } }.replicate

  def czero: Term = lambda("s") { s => lambda("z") { z => z } }.replicate
  def csucc: Term = lambda("n") { n => lambda("s") { s => lambda("z") { z => s(n(s)(z)) } } }.replicate
  def ciszero: Term = lambda("n") { n => n(lambda("x") { x => cfalse })(ctrue) }.replicate

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
  display("iszero(succ(succ(zero)))", ciszero(csucc(csucc(czero))))

  private def display(label: String, term: Term): Unit = {
    println(label)
    printReductions(term)
  }

  private def printReductions(term: Term): Unit = {
    println(term)
    eval1withSubst(term) match {
      case Some((result, v, subst)) =>
        println(term.toUnderlining(Map(v -> '^', subst -> '-')))
        printReductions(result)
      case _ => println()
    }
  }
}
