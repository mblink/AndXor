package andxor

import andxor.types._
import scalaz.{\/, PlusEmpty, Semigroup}

trait FInj[T[_[_]], A[_[_]]] {
  def apply[F[_]: PlusEmpty](af: A[F]): T[F]
}

object FInj {
  def apply[T[_[_]], A[_[_]]](implicit ev: FInj[T, A]): FInj[T, A] = ev

  def instance[A[_[_]], T[_[_]]](f: ForallF[Lambda[f[_] => A[f] => T[f]]]): FInj[T, A] = new FInj[T, A] {
    def apply[F[_]: PlusEmpty](af: A[F]): T[F] = f.apply(af)
  }

  new FInj[Prod1[?[_], Int], Lambda[f[_] => f[Int]]] {
    def apply[F[_]: PlusEmpty](i: F[Int]): Prod1[F, Int] = Prod1(i)
  }

  new FInj[Prod2[?[_], AndXor1[Int], AndXor1[String]], Lambda[f[_] => f[String]]] {
    def apply[F[_]: PlusEmpty](s: F[String]): Prod2[F, AndXor1[Int], AndXor1[String]] =
      Prod2[F, AndXor1[Int], AndXor1[String]]((Prod1[F, Int](PlusEmpty[F].empty[Int]), Prod1[F, String](s)))
  }

  // implicit def fdecidableInj[T[_[_]]]: FDecidable[Lambda[a[_] => FInj[T, Lambda[f[_] => a[f[_]]]]]] =
  //   new FDecidable[Lambda[a[_] => FInj[T, Lambda[f[_] => a[f[_]]]]]] {
  //     def contramap[A[_], B[_]](ta: FInj[T, A])(f: B ~> A): FInj[T, B] = ???
  //     // def contramap[A, B](fa: Inj[Cop, A])(f: B => A): Inj[Cop, B] =
  //     //   instance(b => fa(f(b)))

  //     // def choose2[Z, A1, A2](a1: => Inj[Cop, A1], a2: => Inj[Cop, A2])(f: Z => (A1 \/ A2)): Inj[Cop, Z] =
  //     //   instance(f(_).fold(a1(_), a2(_)))
  //   }

  // implicit def divideInj[Prod](implicit S: Semigroup[Prod]): Divide[Inj[Prod, ?]] =
  //   new Divide[Inj[Prod, ?]] {
  //     def contramap[A, B](fa: Inj[Prod, A])(f: B => A): Inj[Prod, B] =
  //       instance(b => fa(f(b)))

  //     def divide2[A1, A2, Z](a1: => Inj[Prod, A1], a2: => Inj[Prod, A2])(f: Z => (A1, A2)): Inj[Prod, Z] =
  //       instance { z =>
  //         val (i1, i2) = f(z)
  //         S.append(a1(i1), a2(i2))
  //       }
  //   }
}


trait Inj[Cop, A] {
  def apply(a: A): Cop
}

object Inj {
  def apply[Cop, A](implicit ev: Inj[Cop, A]): Inj[Cop, A] = ev

  def instance[A, B](ab: A => B): Inj[B, A] = new Inj[B, A] {
    def apply(a: A): B = ab(a)
  }

  implicit def decidableInj[Cop]: Decidable[Inj[Cop, ?]] =
    new Decidable[Inj[Cop, ?]] {
      def contramap[A, B](fa: Inj[Cop, A])(f: B => A): Inj[Cop, B] =
        instance(b => fa(f(b)))

      def choose2[Z, A1, A2](a1: => Inj[Cop, A1], a2: => Inj[Cop, A2])(f: Z => (A1 \/ A2)): Inj[Cop, Z] =
        instance(f(_).fold(a1(_), a2(_)))
    }

  implicit def divideInj[Prod](implicit S: Semigroup[Prod]): Divide[Inj[Prod, ?]] =
    new Divide[Inj[Prod, ?]] {
      def contramap[A, B](fa: Inj[Prod, A])(f: B => A): Inj[Prod, B] =
        instance(b => fa(f(b)))

      def divide2[A1, A2, Z](a1: => Inj[Prod, A1], a2: => Inj[Prod, A2])(f: Z => (A1, A2)): Inj[Prod, Z] =
        instance { z =>
          val (i1, i2) = f(z)
          S.append(a1(i1), a2(i2))
        }
    }
}
