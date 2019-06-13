package andxor

import scalaz.{Apply, \/}

object Combine {
  def divide2[F[_], A1, A2, Z](a1: => F[A1], a2: => F[A2])(f: Z => (A1, A2))(
    implicit
    D: Divide[F]): F[Z] = D.divide2(a1, a2)(f)

  def choose2[F[_], Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: Z => (A1 \/ A2))(
    implicit
    D: Decidable[F]): F[Z] = D.choose2(a1, a2)(f)

  def altly2[F[_], Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: A1 \/ A2 => Z)(
    implicit
    A: Alt[F]): F[Z] = A.altly2(a1, a2)(f)

  def apply2[F[_], A1, A2, R](a1: => F[A1], a2: => F[A2])(f: (A1, A2) => R)(
    implicit
    A: Apply[F]): F[R] = A.apply2(a1, a2)(f)

}
