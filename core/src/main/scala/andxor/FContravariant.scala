package andxor

import scalaz.~>

trait FContravariant[F[_[_]]] {
  def contramap[A[_], B[_]](fa: F[A])(f: B ~> A): F[B]
}

object FContravariant {
  def apply[T[_[_]]](implicit ev: FContravariant[T]): FContravariant[T] = ev
}
