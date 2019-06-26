package andxor
package syntax

import scalaz.{~>, Applicative}
import scalaz.Leibniz.===

object ftraverse {
  implicit class FTraverseOps[T[_[_]], F[_]](tf: T[F])(implicit T: FTraverse[T]) {
    def traverse[G[_], A[_]: Applicative](f: F ~> Lambda[a => A[G[a]]]): A[T[G]] = T.traverse(tf)(f)

    // TODO - improve inference?
    def sequence[G[_], A[_]: Applicative](implicit ev: T[F] === T[Lambda[a => A[G[a]]]]): A[T[G]] = T.sequence(ev(tf))
  }
}
