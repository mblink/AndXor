package andxor

import scalaz.~>

trait Transform[F[_[_]]] {
  def transform[G[_], H[_]](nt: G ~> H): F[G] => F[H]
}

object Transform {
  def apply[F[_[_]]](implicit ev: Transform[F]): Transform[F] = ev

  object ops {
    implicit class TransformOps[F[_[_]], G[_]](fg: F[G])(implicit T: Transform[F]) {
      def transform[H[_]](nt: G ~> H): F[H] = T.transform(nt)(fg)
    }
  }
}
