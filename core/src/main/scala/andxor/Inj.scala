package andxor

import scalaz.{\/, Monoid}

trait Inj[Cop, A] {
  def apply(a: A): Cop
}

object Inj {
  def instance[A, B](ab: A => B): Inj[B, A] = new Inj[B, A] {
    def apply(a: A): B = ab(a)
  }

  implicit def decidableInj[Cop]: Decidable[Inj[Cop, ?]] =
    new Decidable[Inj[Cop, ?]] {
      def lose[A](f: A => Void): Inj[Cop, A] =
        instance(a => absurd(f(a)))

      def choose2[Z, A1, A2](a1: => Inj[Cop, A1], a2: => Inj[Cop, A2])(f: Z => (A1 \/ A2)): Inj[Cop, Z] =
        instance(f(_).fold(a1(_), a2(_)))
    }

  implicit def divideInj[Prod](implicit M: Monoid[Prod]): Divide[Inj[Prod, ?]] =
    new Divide[Inj[Prod, ?]] {
      def conquer[A]: Inj[Prod, A] =
        instance(_ => M.zero)

      def divide2[A1, A2, Z](a1: => Inj[Prod, A1], a2: => Inj[Prod, A2])(f: Z => (A1, A2)): Inj[Prod, Z] =
        instance { z =>
          val (i1, i2) = f(z)
          M.append(a1(i1), a2(i2))
        }
    }
}

