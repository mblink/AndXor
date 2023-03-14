package andxor
package syntax

import cats.~>
import cats.evidence.===

object ftraverse {
  implicit class FTraverseOps[T[_[_]], TC[_[_]], F[_]](tf: T[F])(implicit T: FTraverse[T, TC]) {
    def traverse[G[_], A[_]: TC](f: F ~> ([a] =>> A[G[a]])): A[T[G]] = T.traverse(tf)(f)

    // TODO - improve inference?
    def sequence[G[_], A[_]: TC](implicit ev: T[F] === T[[a] =>> A[G[a]]]): A[T[G]] = T.sequence(ev.coerce(tf))
  }
}
