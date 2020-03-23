package andxor

import cats.{Monoid, Semigroup}

trait Inj[Cop, A] {
  def apply(a: A): Cop
}

trait InjLP {
  trait DivideInj[Prod] extends Divide[Inj[Prod, ?]] {
    def G: Semigroup[Prod]
    override def contramap[A, B](fa: Inj[Prod, A])(f: B => A): Inj[Prod, B] = Inj.instance(b => fa(f(b)))
    def divide2[A1, A2, Z](a1: Inj[Prod, A1], a2: Inj[Prod, A2])(f: Z => (A1, A2)): Inj[Prod, Z] =
      Inj.instance { z =>
        val (i1, i2) = f(z)
        G.combine(a1(i1), a2(i2))
      }
  }

  implicit def divideInj[Prod](implicit S: Semigroup[Prod]): Divide[Inj[Prod, ?]] = new DivideInj[Prod] { val G = S }
}

object Inj extends InjLP {
  def apply[Cop, A](implicit ev: Inj[Cop, A]): Inj[Cop, A] = ev

  def instance[A, B](ab: A => B): Inj[B, A] = new Inj[B, A] {
    def apply(a: A): B = ab(a)
  }

  implicit def decidableInj[Cop]: Decidable[Inj[Cop, ?]] =
    new Decidable[Inj[Cop, ?]] {
      def contramap[A, B](fa: Inj[Cop, A])(f: B => A): Inj[Cop, B] = instance(b => fa(f(b)))
      def choose2[Z, A1, A2](a1: Inj[Cop, A1], a2: Inj[Cop, A2])(f: Z => Either[A1, A2]): Inj[Cop, Z] =
        instance(f(_).fold(a1(_), a2(_)))
    }

  implicit def divisibleInj[Prod](implicit M: Monoid[Prod]): Divisible[Inj[Prod, ?]] =
    new Divisible[Inj[Prod, ?]] with DivideInj[Prod] {
      val G = M
      def conquer[A]: Inj[Prod, A] = instance(_ => M.empty)
    }
}
