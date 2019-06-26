package andxor

import andxor.types._
import scalaz.{Apply, Monoid, \/}
import scalaz.Id.Id
import scalaz.std.vector._

trait AndXorNested9[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]]] extends AndXor {

  def apply[B1]: AndXorNested10[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T] = AndXorNested10[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T]
  def nest[B1[_[_]]]: AndXorNested10[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1] = AndXorNested10[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1]

  def apply[B1, B2]: AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T] = AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2] = AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2]

  def apply[B1, B2, B3]: AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T] = AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]]]: AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3] = AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T] = AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]]]: AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4] = AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4]

  def apply[B1, B2, B3, B4, B5]: AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T] = AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]]]: AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4, B5] = AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4, B5]

  def apply[B1, B2, B3, B4, B5, B6]: AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T] = AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]]]: AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4, B5, B6] = AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4, B5, B6]

  def apply[B1, B2, B3, B4, B5, B6, B7]: AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T] = AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]]]: AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4, B5, B6, B7] = AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4, B5, B6, B7]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8]: AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T] = AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]]]: AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4, B5, B6, B7, B8] = AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4, B5, B6, B7, B8]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9]: AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T] = AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]]]: AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4, B5, B6, B7, B8, B9]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]: AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T] = AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]]]: AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]: AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T] = AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]]]: AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] = AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]: AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T] = AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]]]: AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] = AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]: AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T] = AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]]]: AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13] = AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]

  type Prod[F[_]] = Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]
  object Prod {
    def apply[F[_]](p: (A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F])): Prod[F] = Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](p)
  }

  type Cop[F[_]] = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]
  object Cop {
    def apply[F[_]](c: (A1[F] \/ (A2[F] \/ (A3[F] \/ (A4[F] \/ (A5[F] \/ (A6[F] \/ (A7[F] \/ (A8[F] \/ A9[F]))))))))): Cop[F] = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](c)
  }

  def deriving[TC[_], F[_]](implicit t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =
    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose9(t0, t1, t2, t3, t4, t5, t6, t7, t8)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly9(t0, t1, t2, t3, t4, t5, t6, t7, t8)(x => f(Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide9(t0, t1, t2, t3, t4, t5, t6, t7, t8)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply9(t0, t1, t2, t3, t4, t5, t6, t7, t8) {
          case (i0, i1, i2, i3, i4, i5, i6, i7, i8) =>
            f(Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((i0, i1, i2, i3, i4, i5, i6, i7, i8)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1[Id]], t1: TC[A2[Id]], t2: TC[A3[Id]], t3: TC[A4[Id]], t4: TC[A5[Id]], t5: TC[A6[Id]], t6: TC[A7[Id]], t7: TC[A8[Id]], t8: TC[A9[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = deriving[Inj[Prod[F], ?], F].divide
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): FInj[Prod, Cop, F] = deriving[Inj[Prod[F], ?], F].choose
    implicit def injProdToVecCopEv[F[_]]: FInj[Lambda[f[_] => Vector[Cop[f]]], Prod, F] = deriving[Inj[Vector[Cop[F]], ?], F].divide
  }
}

object AndXorNested9 {
  def apply[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]]]: AndXorNested9[A1, A2, A3, A4, A5, A6, A7, A8, A9] =
    new AndXorNested9[A1, A2, A3, A4, A5, A6, A7, A8, A9] {}
}

trait AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9] extends AndXorNested9[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T] {
  def derivingId[TC[_]](implicit dumb: DummyImplicit, t0: TC[A1], t1: TC[A2], t2: TC[A3], t3: TC[A4], t4: TC[A5], t5: TC[A6], t6: TC[A7], t7: TC[A8], t8: TC[A9]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]
}

object AndXor9 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9]: AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9] =
    new AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9] {}
}
