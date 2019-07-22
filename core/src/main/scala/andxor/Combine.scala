package andxor

import andxor.tuple._
import scalaz.{Apply, \/}

object Combine {
  def divide2[F[_], A1, A2, Z](a1: => F[A1], a2: => F[A2])(f: Z => (A1, A2))(
    implicit
    D: Divide[F]): F[Z] = D.divide2(a1, a2)(f)

  def divide3[F[_], A1, A2, A3, Z](a0: => F[A1], a1: => F[A2], a2: => F[A3])(
    f: Z => (A1, A2, A3))(implicit D: Divide[F]): F[Z] = D.divide2(
    a0, D.tuple2(a1, a2)) { z =>
    val t = f(z)
    (t.t1, (t.t2, t.t3))
  }

  def divide4[F[_], A1, A2, A3, A4, Z](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4])(
    f: Z => (A1, A2, A3, A4))(implicit D: Divide[F]): F[Z] = D.divide2(
    a0, D.tuple2(a1, D.tuple2(a2, a3))) { z =>
    val t = f(z)
    (t.t1, (t.t2, (t.t3, t.t4)))
  }

  def choose2[F[_], Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: Z => (A1 \/ A2))(
    implicit
    D: Decidable[F]): F[Z] = D.choose2(a1, a2)(f)

  def choose3[F[_], Z, A1, A2, A3](a0: => F[A1], a1: => F[A2], a2: => F[A3])(
    f: Z => (A1 \/ (A2 \/ A3)))(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ A3)] = D.choose2(a1, a2)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose4[F[_], Z, A1, A2, A3, A4](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4])(
    f: Z => (A1 \/ (A2 \/ (A3 \/ A4))))(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ (A3 \/ A4))] = choose3(a1, a2, a3)(identity)
    D.choose2(a0, tail)(f)
  }

  def altly2[F[_], Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: A1 \/ A2 => Z)(
    implicit
    A: Alt[F]): F[Z] = A.altly2(a1, a2)(f)

  def altly3[F[_], Z, A1, A2, A3](a0: => F[A1], a1: => F[A2], a2: => F[A3])(
    f: (A1 \/ (A2 \/ A3)) => Z)(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, A.altly2(a1, a2)(identity))(f)

  def altly4[F[_], Z, A1, A2, A3, A4](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4])(
    f: (A1 \/ (A2 \/ (A3 \/ A4))) => Z)(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly3(a1, a2, a3)(identity))(f)

  def apply2[F[_], A1, A2, R](a1: => F[A1], a2: => F[A2])(f: (A1, A2) => R)(
    implicit
    A: Apply[F]): F[R] = A.apply2(a1, a2)(f)

  def apply3[F[_], A1, A2, A3, R](fa0: => F[A1], fa1: => F[A2], fa2: => F[A3])(
    f: (A1, A2, A3) => R)(
    implicit
    A: Apply[F]): F[R] = {
    import A._
    ap(fa2)(ap(fa1)(map(fa0)(a0 => a1 => a2 => f(a0, a1, a2))))
  }

  def apply4[F[_], A1, A2, A3, A4, R](fa0: => F[A1], fa1: => F[A2], fa2: => F[A3], fa3: => F[A4])(
    f: (A1, A2, A3, A4) => R)(
    implicit
    A: Apply[F]): F[R] = {
    import A._
    ap(fa3)(ap(fa2)(ap(fa1)(map(fa0)(a0 => a1 => a2 => a3 => f(a0, a1, a2, a3)))))
  }

}
