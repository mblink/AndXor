package andxor

import andxor.types._
import scalaz.{Apply, Monoid, \/}
import scalaz.Id.Id
import scalaz.std.vector._

trait AndXor5[A1, A2, A3, A4, A5] extends AndXor {

  def apply[B1]: AndXor6[A1, A2, A3, A4, A5, B1] = AndXor6[A1, A2, A3, A4, A5, B1]

  def apply[B1, B2]: AndXor7[A1, A2, A3, A4, A5, B1, B2] = AndXor7[A1, A2, A3, A4, A5, B1, B2]

  def apply[B1, B2, B3]: AndXor8[A1, A2, A3, A4, A5, B1, B2, B3] = AndXor8[A1, A2, A3, A4, A5, B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXor9[A1, A2, A3, A4, A5, B1, B2, B3, B4] = AndXor9[A1, A2, A3, A4, A5, B1, B2, B3, B4]

  def apply[B1, B2, B3, B4, B5]: AndXor10[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5] = AndXor10[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5]

  def apply[B1, B2, B3, B4, B5, B6]: AndXor11[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6] = AndXor11[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6]

  def apply[B1, B2, B3, B4, B5, B6, B7]: AndXor12[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7] = AndXor12[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8]: AndXor13[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8] = AndXor13[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9]: AndXor14[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXor14[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]: AndXor15[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXor15[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]: AndXor16[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] = AndXor16[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]: AndXor17[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] = AndXor17[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]: AndXor18[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13] = AndXor18[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]: AndXor19[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14] = AndXor19[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]: AndXor20[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15] = AndXor20[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]: AndXor21[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16] = AndXor21[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17]: AndXor22[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17] = AndXor22[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17]

  type Prod[F[_]] = Prod5[F, A1, A2, A3, A4, A5]
  object Prod {
    def apply[F[_]](p: (F[A1], F[A2], F[A3], F[A4], F[A5])): Prod[F] = Prod5[F, A1, A2, A3, A4, A5](p)
  }

  type Cop[F[_]] = Cop5[F, A1, A2, A3, A4, A5]
  object Cop {
    def apply[F[_]](c: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ F[A5]))))): Cop[F] = Cop5[F, A1, A2, A3, A4, A5](c)
  }

  def deriving[TC[_], F[_]](implicit t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]]): AndXorDeriving[TC, Cop[F], Prod[F]] =
    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose5(t0, t1, t2, t3, t4)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly5(t0, t1, t2, t3, t4)(x => f(Cop5[F, A1, A2, A3, A4, A5](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide5(t0, t1, t2, t3, t4)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply5(t0, t1, t2, t3, t4) {
          case (i0, i1, i2, i3, i4) =>
            f(Prod5[F, A1, A2, A3, A4, A5]((i0, i1, i2, i3, i4)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1], t1: TC[A2], t2: TC[A3], t3: TC[A4], t4: TC[A5]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = deriving[Inj[Prod[F], ?], F].divide
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]] = deriving[Inj[Prod[F], ?], F].choose
    implicit def injProdToVecCopEv[F[_]]: Inj[Vector[Cop[F]], Prod[F]] = deriving[Inj[Vector[Cop[F]], ?], F].divide
  }
}

object AndXor5 {
  def apply[A1, A2, A3, A4, A5]: AndXor5[A1, A2, A3, A4, A5] =
    new AndXor5[A1, A2, A3, A4, A5] {}
}
