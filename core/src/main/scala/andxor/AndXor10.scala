package andxor

import andxor.types._
import scalaz.{Apply, Monoid, \/}
import scalaz.Id.Id
import scalaz.std.vector._

trait AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] extends AndXor {

  def apply[B1]: AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1] = AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1]

  def apply[B1, B2]: AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2] = AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2]

  def apply[B1, B2, B3]: AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3] = AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4] = AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4]

  def apply[B1, B2, B3, B4, B5]: AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5] = AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5]

  def apply[B1, B2, B3, B4, B5, B6]: AndXor16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6] = AndXor16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6]

  def apply[B1, B2, B3, B4, B5, B6, B7]: AndXor17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7] = AndXor17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8]: AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8] = AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9]: AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]: AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]: AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] = AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]: AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] = AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]

  type Prod[F[_]] = Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]
  object Prod {
    def apply[F[_]](p: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10])): Prod[F] = Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](p)
  }

  type Cop[F[_]] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]
  object Cop {
    def apply[F[_]](c: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ F[A10])))))))))): Cop[F] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](c)
  }

  def deriving[TC[_], F[_]](implicit t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]]): AndXorDeriving[TC, Cop[F], Prod[F]] =
    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose10(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly10(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9)(x => f(Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide10(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply10(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9) {
          case (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9) =>
            f(Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1], t1: TC[A2], t2: TC[A3], t3: TC[A4], t4: TC[A5], t5: TC[A6], t6: TC[A7], t7: TC[A8], t8: TC[A9], t9: TC[A10]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = deriving[Inj[Prod[F], ?], F].divide
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]] = deriving[Inj[Prod[F], ?], F].choose
    implicit def injProdToVecCopEv[F[_]]: Inj[Vector[Cop[F]], Prod[F]] = deriving[Inj[Vector[Cop[F]], ?], F].divide
  }
}

object AndXor10 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] =
    new AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {}
}
