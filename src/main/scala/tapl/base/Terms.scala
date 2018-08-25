package tapl.base

trait Terms {
  type T <: Rewritable // bound to the concrete term type
  type Mapping = Map[T, T]
  type Highlights = Map[Rewritable, Char]

  trait Rewritable {
    val isValue = false
    def replicateUsing(mapping: Mapping): T
    def replicate: T = replicateUsing(Map())
    def rewriteUsing(mapping: Mapping): T = mapping.getOrElse(this.asInstanceOf[T], replicateUsing(mapping))
    def toUnderlining(highlights: Highlights): String = highlights get this match {
      case Some(c) => c.toString * toString.length
      case None => underlinedParts(highlights)
    }
    def underlinedParts(highlights: Highlights): String = " " * toString.length
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
    override def underlinedParts(highlights: Highlights): String = fun match {
      case _: Abs => s" ${fun.toUnderlining(highlights)}  ${arg.toUnderlining(highlights)}"
      case _: App => s" ${fun.toUnderlining(highlights)}  ${arg.toUnderlining(highlights)}"
      case _ => s"${fun.toUnderlining(highlights)} ${arg.toUnderlining(highlights)}"
    }
  }

  trait Abs extends Rewritable {
    val variable: Var
    val body: T
    override val isValue = true
    override def toString: String = s"\u03BB$variable. $body"

    override def underlinedParts(highlights: Highlights): String = {
      val varMark = (if (highlights contains variable) '*' else ' ').toString
      s" ${varMark.toString * variable.toString.length}  ${body.toUnderlining(highlights)}"
    }
  }

  trait Literal[V] extends Rewritable {
    val value: V
    override val isValue = true
    override def toString: String = value.toString
  }
}
