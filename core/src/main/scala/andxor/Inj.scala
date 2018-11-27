package andxor

import scala.language.higherKinds
import scalaz.{Applicative, Functor, Semigroup, \/}

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

  def inject[Cop, A](a: A)(implicit inj: Inj[Cop, A]): Cop = inj(a)

  implicit def decidableInj[Cop]: Decidable[Aux[Cop]#Out] =
    new Decidable[Aux[Cop]#Out] {
      type I[A] = Aux[Cop]#Out[A]
      def choose2[Z, A1, A2](a1: =>I[A1], a2: =>I[A2])(f: Z => (A1 \/ A2)): I[Z] =
        new Inj[Cop, Z] {
          def apply(z: Z): Cop = f(z).fold(a1.apply(_), a2.apply(_))
        }
    }

  implicit def divideInj[Prod](implicit S: Semigroup[Prod]): Divide[Aux[Prod]#Out] =
    new Divide[Aux[Prod]#Out] {
      type I[A] = Aux[Prod]#Out[A]
      def contramap[A, B](ia: I[A])(f: B => A): I[B] = new Inj[Prod, B] {
        def apply(b: B): Prod = ia(f(b))
      }
      def divide2[A1, A2, Z](a1: =>I[A1], a2: =>I[A2])(f: Z => (A1, A2)): I[Z] =
        new Inj[Prod, Z] {
          def apply(z: Z): Prod = {
            val (i1, i2) = f(z)
            S.append(a1(i1), a2(i2))
          }
        }
    }

  implicit def apInjA[F[_], Cop, A](implicit F: Applicative[F], inj: Inj[Cop, A]): Inj[F[Cop], A] = new Inj[F[Cop], A] {
    def apply(a: A): F[Cop] = F.point(inj(a))
  }

  implicit def fnInjA[F[_], Cop, A](implicit F: Functor[F], inj: Inj[Cop, A]): Inj[F[Cop], F[A]] = new Inj[F[Cop], F[A]] {
    def apply(a: F[A]): F[Cop] = F.map(a)(inj(_))
  }
}

