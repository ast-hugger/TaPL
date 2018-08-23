package tapl.base

/**
  *
  * @tparam T The root of the term class hierarchy which mixes in
  *           [[Rewritable]].
  */
abstract class Terms[T] {
  type Mapping = Map[T, T]

  trait Rewritable {
    val isValue = false
    def replicateUsing(mapping: Mapping): T
    def rewriteUsing(mapping: Mapping): T = mapping.getOrElse(this.asInstanceOf[T], replicateUsing(mapping))
  }

  trait Var extends Rewritable {
    val name: String
    override def toString: String = name
  }

  trait App extends Rewritable {
    val fun: T
    val arg: T
    override def toString: String = fun match {
      case _: Abs => s"($fun) $arg"
      case _: App => s"($fun) $arg"
      case _ => s"$fun $arg"
    }
  }

  trait Abs extends Rewritable {
    val variable: Var
    val body: T
    override val isValue = true
    override def toString: String = s"\u03BB$variable. $body"
  }

  trait Literal[V] extends Rewritable {
    val value: V
    override val isValue = true
    override def toString: String = value.toString
  }
}
