package andxor
package syntax

import scalaz.~>

object ffunctor {
  implicit class FFunctorOps[F[_[_]], A[_]](fa: F[A])(implicit F: FFunctor[F]) {
    def map[B[_]](f: A ~> B): F[B] = F.map(fa)(f)
  }
}
