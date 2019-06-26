package andxor

import scalaz.{~>, Equal}

trait FFunctor[F[_[_]]] {
  def map[A[_], B[_]](fa: F[A])(f: A ~> B): F[B]

  trait FFunctorLaw {
    def identity[A[_]](fa: F[A])(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(map(fa)(new (A ~> A) { def apply[a](aa: A[a]): A[a] = aa }), fa)

    def composite[A[_], B[_], C[_]](fa: F[A], f1: A ~> B, f2: B ~> C)(implicit FC: Equal[F[C]]): Boolean =
      FC.equal(map(map(fa)(f1))(f2), map(fa)(f2.compose(f1)))
  }
  def ffunctorLaw = new FFunctorLaw {}
}

object FFunctor {
  def apply[F[_[_]]](implicit ev: FFunctor[F]): FFunctor[F] = ev
}
