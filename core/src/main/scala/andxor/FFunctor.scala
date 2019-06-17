package andxor

import scalaz.~>

trait FFunctor[F[_[_]]] {
  def map[A[_], B[_]](fa: F[A])(f: A ~> B): F[B]
}

object FFunctor {
  def apply[F[_[_]]](implicit ev: FFunctor[F]): FFunctor[F] = ev
}
