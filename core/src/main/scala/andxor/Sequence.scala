package andxor

import scalaz.Id.Id

trait Sequence[F[_[_]], Ap[_[_]]] {
  def sequence[G[_]](fg: F[G])(implicit G: Ap[G]): G[F[Id]]
}

object Sequence {
  def apply[F[_[_]], Ap[_[_]]](implicit ev: Sequence[F, Ap]): Sequence[F, Ap] = ev

  object syntax {
    implicit class SequenceOps[F[_[_]], Ap[_[_]], G[_]](fg: F[G])(implicit S: Sequence[F, Ap]) {
      def sequence(implicit G: Ap[G]): G[F[Id]] = S.sequence(fg)
    }
  }
}
