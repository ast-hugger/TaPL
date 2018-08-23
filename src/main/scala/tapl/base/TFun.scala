package tapl.base

case class TFun(from: Type, to: Type) extends Type {
  override def toString: String = from match {
    case _: TFun => s"($from) -> $to"
    case _ => s"$from -> $to"
  }
}
