package tapl.base

object Util {

  /**
    * Convert a collection of [[Either]]s into an Either which, if all elements
    * of the original collection are [[Right]]s, is a Right with a list of their
    * values. Otherwise, return a [[Left]] with the first encountered Left.
    */
  def allRightsOrLeft[A, B](xs: Iterable[Either[A, B]]): Either[A, Iterable[B]] = {
    val either = xs.foldLeft(Right(Nil): Either[A, List[B]]) {
      case (Right(res), Right(x)) => Right(x :: res)
      case (Right(_), Left(err)) => Left(err)
      case (Left(err), _) => Left(err)
    }
    either.map(_.reverse)
  }
}
