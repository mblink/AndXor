package andxor

import java.lang.Void
import scalaz.{\/, Monoid}

trait Inj[Cop, A] {
  def apply(a: A): Cop
}

object Inj {

  trait Aux[Cop] {
    type Out[A] = Inj[Cop, A]
  }

  def instance[A, B](ab: A => B): Inj[B, A] = new Inj[B, A] {
    def apply(a: A): B = ab(a)
  }

  implicit def decidableInj[Cop]: Decidable[Aux[Cop]#Out] =
    new Decidable[Aux[Cop]#Out] {
      type I[A] = Aux[Cop]#Out[A]

      def lose[A](f: A => Void): I[A] =
        instance(a => absurd(f(a)))

      def choose2[Z, A1, A2](a1: =>I[A1], a2: =>I[A2])(f: Z => (A1 \/ A2)): I[Z] =
        instance(f(_).fold(a1.apply(_), a2.apply(_)))
    }

  implicit def divideInj[Prod](implicit M: Monoid[Prod]): Divide[Aux[Prod]#Out] =
    new Divide[Aux[Prod]#Out] {
      type I[A] = Aux[Prod]#Out[A]

      def conquer[A]: I[A] = instance(_ => M.zero)

      def divide2[A1, A2, Z](a1: =>I[A1], a2: =>I[A2])(f: Z => (A1, A2)): I[Z] =
        instance { z =>
          val (i1, i2) = f(z)
          M.append(a1(i1), a2(i2))
        }
    }
}

