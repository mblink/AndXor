package andxor

import andxor.types._
import scalaz.{Apply, Monoid, \/}
import scalaz.Id.Id
import scalaz.std.vector._

trait AndXorNested12[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]]] extends AndXor {

  def apply[B1]: AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, FConst[B1]#T] = AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, FConst[B1]#T]
  def nest[B1[_[_]]]: AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B1] = AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B1]

  def apply[B1, B2]: AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, FConst[B1]#T, FConst[B2]#T] = AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, FConst[B1]#T, FConst[B2]#T]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B1, B2] = AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B1, B2]

  def apply[B1, B2, B3]: AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T] = AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]]]: AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B1, B2, B3] = AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T] = AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]]]: AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B1, B2, B3, B4] = AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B1, B2, B3, B4]

  def apply[B1, B2, B3, B4, B5]: AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T] = AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]]]: AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B1, B2, B3, B4, B5] = AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B1, B2, B3, B4, B5]

  def apply[B1, B2, B3, B4, B5, B6]: AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T] = AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]]]: AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B1, B2, B3, B4, B5, B6] = AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B1, B2, B3, B4, B5, B6]

  def apply[B1, B2, B3, B4, B5, B6, B7]: AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T] = AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]]]: AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B1, B2, B3, B4, B5, B6, B7] = AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B1, B2, B3, B4, B5, B6, B7]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8]: AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T] = AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]]]: AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B1, B2, B3, B4, B5, B6, B7, B8] = AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B1, B2, B3, B4, B5, B6, B7, B8]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9]: AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T] = AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]]]: AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B1, B2, B3, B4, B5, B6, B7, B8, B9]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]: AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T] = AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]]]: AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]

  type Prod[F[_]] = Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]
  object Prod {
    def apply[F[_]](p: (A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F])): Prod[F] = Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](p)
  }

  type Cop[F[_]] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]
  object Cop {
    def apply[F[_]](c: (A1[F] \/ (A2[F] \/ (A3[F] \/ (A4[F] \/ (A5[F] \/ (A6[F] \/ (A7[F] \/ (A8[F] \/ (A9[F] \/ (A10[F] \/ (A11[F] \/ A12[F])))))))))))): Cop[F] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](c)
  }

  def deriving[TC[_], F[_]](implicit t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =
    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose12(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly12(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)(x => f(Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide12(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply12(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) {
          case (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11) =>
            f(Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1[Id]], t1: TC[A2[Id]], t2: TC[A3[Id]], t3: TC[A4[Id]], t4: TC[A5[Id]], t5: TC[A6[Id]], t6: TC[A7[Id]], t7: TC[A8[Id]], t8: TC[A9[Id]], t9: TC[A10[Id]], t10: TC[A11[Id]], t11: TC[A12[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = deriving[Inj[Prod[F], ?], F].divide
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): FInj[Prod, Cop, F] = deriving[Inj[Prod[F], ?], F].choose
    implicit def injProdToVecCopEv[F[_]]: FInj[Lambda[f[_] => Vector[Cop[f]]], Prod, F] = deriving[Inj[Vector[Cop[F]], ?], F].divide
  }
}

object AndXorNested12 {
  def apply[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]]]: AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] =
    new AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}
}

trait AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] extends AndXorNested12[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T] {
  def derivingId[TC[_]](implicit dumb: DummyImplicit, t0: TC[A1], t1: TC[A2], t2: TC[A3], t3: TC[A4], t4: TC[A5], t5: TC[A6], t6: TC[A7], t7: TC[A8], t8: TC[A9], t9: TC[A10], t10: TC[A11], t11: TC[A12]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]
}

object AndXor12 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] =
    new AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}
}
