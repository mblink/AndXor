package andxor

import cats.{~>, Functor}
import cats.syntax.either.*

trait Alt[F[_]] extends Functor[F] {
  def alt[A](a1: F[A], a2: F[A]): F[A]
  def altly2[Z, A1, A2](a1: F[A1], a2: F[A2])(f: Either[A1, A2] => Z): F[Z] =
    map(alt(map(a1)(_.asLeft[A2]), map(a2)(_.asRight[A1])))(f)
}

object Alt {
  def fromIso[F[_], G[_]](fg: F ~> G, gf: G ~> F)(implicit G: Alt[G]): Alt[F] =
    new Alt[F] {
      def map[A, B](fa: F[A])(f: A => B): F[B] = gf(G.map(fg(fa))(f))
      def alt[A](a1: F[A], a2: F[A]): F[A] = gf(G.alt(fg(a1), fg(a2)))
    }
}
