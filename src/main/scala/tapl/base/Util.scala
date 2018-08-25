package tapl.base

object Util {

  def mapAndJoin[T, S, R](xs: Iterable[T])(f: T => S)(g: (S, S) => S): S = {
    val s = xs.toList.map(f)
    s.tail.foldLeft(s.head)(g)
  }
}
