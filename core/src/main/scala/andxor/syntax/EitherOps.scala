package andxor
package syntax

trait EitherSyntax {
  private[andxor] implicit class EitherOps[A, B](e: Either[A, B]) {
    def bimap[C, D](f: A => C, g: B => D): Either[C, D] = e match {
      case Left(a) => Left(f(a))
      case Right(b) => Right(g(b))
    }

    def leftMap[C](f: A => C): Either[C, B] =
      bimap(f, identity[B])
  }

  private[andxor] implicit class EitherIdOps[A](a: A) {
    def left[B]: Either[A, B] = Left(a)
    def right[B]: Either[B, A] = Right(a)
  }
}
