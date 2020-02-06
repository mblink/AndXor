package andxor

import andxor.tuple._

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

  def divide5[F[_], A1, A2, A3, A4, A5, Z](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5])(
    f: Z => (A1, A2, A3, A4, A5))(implicit D: Divide[F]): F[Z] = D.divide2(
    a0, D.tuple2(a1, D.tuple2(a2, D.tuple2(a3, a4)))) { z =>
    val t = f(z)
    (t.t1, (t.t2, (t.t3, (t.t4, t.t5))))
  }

  def divide6[F[_], A1, A2, A3, A4, A5, A6, Z](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5], a5: => F[A6])(
    f: Z => (A1, A2, A3, A4, A5, A6))(implicit D: Divide[F]): F[Z] = D.divide2(
    a0, D.tuple2(a1, D.tuple2(a2, D.tuple2(a3, D.tuple2(a4, a5))))) { z =>
    val t = f(z)
    (t.t1, (t.t2, (t.t3, (t.t4, (t.t5, t.t6)))))
  }

  def choose2[F[_], Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: Z => Either[A1, A2])(
    implicit
    D: Decidable[F]): F[Z] = D.choose2(a1, a2)(f)

  def choose3[F[_], Z, A1, A2, A3](a0: => F[A1], a1: => F[A2], a2: => F[A3])(
    f: Z => Either[A1, Either[A2, A3]])(implicit D: Decidable[F]): F[Z] = {
    val tail: F[Either[A2, A3]] = D.choose2(a1, a2)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose4[F[_], Z, A1, A2, A3, A4](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4])(
    f: Z => Either[A1, Either[A2, Either[A3, A4]]])(implicit D: Decidable[F]): F[Z] = {
    val tail: F[Either[A2, Either[A3, A4]]] = choose3(a1, a2, a3)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose5[F[_], Z, A1, A2, A3, A4, A5](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5])(
    f: Z => Either[A1, Either[A2, Either[A3, Either[A4, A5]]]])(implicit D: Decidable[F]): F[Z] = {
    val tail: F[Either[A2, Either[A3, Either[A4, A5]]]] = choose4(a1, a2, a3, a4)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose6[F[_], Z, A1, A2, A3, A4, A5, A6](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5], a5: => F[A6])(
    f: Z => Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]])(implicit D: Decidable[F]): F[Z] = {
    val tail: F[Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]] = choose5(a1, a2, a3, a4, a5)(identity)
    D.choose2(a0, tail)(f)
  }

  def altly2[F[_], Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: Either[A1, A2] => Z)(
    implicit
    A: Alt[F]): F[Z] = A.altly2(a1, a2)(f)

  def altly3[F[_], Z, A1, A2, A3](a0: => F[A1], a1: => F[A2], a2: => F[A3])(
    f: Either[A1, Either[A2, A3]] => Z)(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, A.altly2(a1, a2)(identity))(f)

  def altly4[F[_], Z, A1, A2, A3, A4](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4])(
    f: Either[A1, Either[A2, Either[A3, A4]]] => Z)(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly3(a1, a2, a3)(identity))(f)

  def altly5[F[_], Z, A1, A2, A3, A4, A5](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5])(
    f: Either[A1, Either[A2, Either[A3, Either[A4, A5]]]] => Z)(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly4(a1, a2, a3, a4)(identity))(f)

  def altly6[F[_], Z, A1, A2, A3, A4, A5, A6](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5], a5: => F[A6])(
    f: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]] => Z)(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly5(a1, a2, a3, a4, a5)(identity))(f)

  def apply2[F[_], A1, A2, R](a1: => F[A1], a2: => F[A2])(f: (A1, A2) => R)(
    implicit
    A: Apply[F]): F[R] = A.ap(a2)(A.map(a1)(a1 => a2 => f(a1, a2)))

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

  def apply5[F[_], A1, A2, A3, A4, A5, R](fa0: => F[A1], fa1: => F[A2], fa2: => F[A3], fa3: => F[A4], fa4: => F[A5])(
    f: (A1, A2, A3, A4, A5) => R)(
    implicit
    A: Apply[F]): F[R] = {
    import A._
    ap(fa4)(ap(fa3)(ap(fa2)(ap(fa1)(map(fa0)(a0 => a1 => a2 => a3 => a4 => f(a0, a1, a2, a3, a4))))))
  }

  def apply6[F[_], A1, A2, A3, A4, A5, A6, R](fa0: => F[A1], fa1: => F[A2], fa2: => F[A3], fa3: => F[A4], fa4: => F[A5], fa5: => F[A6])(
    f: (A1, A2, A3, A4, A5, A6) => R)(
    implicit
    A: Apply[F]): F[R] = {
    import A._
    ap(fa5)(ap(fa4)(ap(fa3)(ap(fa2)(ap(fa1)(map(fa0)(a0 => a1 => a2 => a3 => a4 => a5 => f(a0, a1, a2, a3, a4, a5)))))))
  }

}
