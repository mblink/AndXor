package andxor

import scalaz.{~>, Applicative}

trait FTraverse0[T[_[_]]] extends FFunctor[T] {
  def traverse[F[_], G[_], A[_]: Applicative](tf: T[F])(f: F ~> Lambda[a => A[G[a]]]): A[T[G]]
  def sequence[F[_], A[_]: Applicative](taf: T[Lambda[a => A[F[a]]]]): A[T[F]]
}

trait FTraverse[T[_[_]]] extends FTraverse0[T] {
  def sequence[F[_], A[_]: Applicative](taf: T[Lambda[a => A[F[a]]]]): A[T[F]] =
    traverse[Lambda[a => A[F[a]]], F, A](taf)(new (Lambda[a => A[F[a]]] ~> Lambda[a => A[F[a]]]) {
      def apply[a](a: A[F[a]]): A[F[a]] = a
    })
}

trait FSequence[T[_[_]]] extends FTraverse0[T] {
  def traverse[F[_], G[_], A[_]: Applicative](tf: T[F])(f: F ~> Lambda[a => A[G[a]]]): A[T[G]] =
    sequence(map[F, Lambda[a => A[G[a]]]](tf)(f))
}

trait FTraverse0Companion { def apply[T[_[_]]](implicit ev: FTraverse0[T]): FTraverse0[T] = ev }
object FTraverse extends FTraverse0Companion
object FSequence extends FTraverse0Companion
