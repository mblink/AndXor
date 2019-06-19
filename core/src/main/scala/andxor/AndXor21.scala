package andxor

import andxor.types._
import scalaz.{Apply, Monoid, \/}
import scalaz.Id.Id

trait AndXorNested21[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]]] extends AndXor {
  type Prod[F[_]] = Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
  object Prod {
    def apply[F[_]](p: (A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F], A21[F])): Prod[F] = Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](p)
  }

  type Cop[F[_]] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
  object Cop {
    def apply[F[_]](c: (A1[F] \/ (A2[F] \/ (A3[F] \/ (A4[F] \/ (A5[F] \/ (A6[F] \/ (A7[F] \/ (A8[F] \/ (A9[F] \/ (A10[F] \/ (A11[F] \/ (A12[F] \/ (A13[F] \/ (A14[F] \/ (A15[F] \/ (A16[F] \/ (A17[F] \/ (A18[F] \/ (A19[F] \/ (A20[F] \/ A21[F]))))))))))))))))))))): Cop[F] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](c)
  }

  def deriving[TC[_], F[_]](implicit t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]], t20: TC[A21[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =
    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose21(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly21(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20)(x => f(Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide21(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply21(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) {
          case (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20) =>
            f(Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1[Id]], t1: TC[A2[Id]], t2: TC[A3[Id]], t3: TC[A4[Id]], t4: TC[A5[Id]], t5: TC[A6[Id]], t6: TC[A7[Id]], t7: TC[A8[Id]], t8: TC[A9[Id]], t9: TC[A10[Id]], t10: TC[A11[Id]], t11: TC[A12[Id]], t12: TC[A13[Id]], t13: TC[A14[Id]], t14: TC[A15[Id]], t15: TC[A16[Id]], t16: TC[A17[Id]], t17: TC[A18[Id]], t18: TC[A19[Id]], t19: TC[A20[Id]], t20: TC[A21[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] =
      deriving[Inj[Prod[F], ?], F].divide
  }
}

object AndXorNested21 {
  def apply[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]]]: AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
    new AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {}
}

trait AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] extends AndXorNested21[AndXorConst[A1]#AXO, AndXorConst[A2]#AXO, AndXorConst[A3]#AXO, AndXorConst[A4]#AXO, AndXorConst[A5]#AXO, AndXorConst[A6]#AXO, AndXorConst[A7]#AXO, AndXorConst[A8]#AXO, AndXorConst[A9]#AXO, AndXorConst[A10]#AXO, AndXorConst[A11]#AXO, AndXorConst[A12]#AXO, AndXorConst[A13]#AXO, AndXorConst[A14]#AXO, AndXorConst[A15]#AXO, AndXorConst[A16]#AXO, AndXorConst[A17]#AXO, AndXorConst[A18]#AXO, AndXorConst[A19]#AXO, AndXorConst[A20]#AXO, AndXorConst[A21]#AXO] {
  override def derivingId[TC[_]](implicit t0: TC[A1], t1: TC[A2], t2: TC[A3], t3: TC[A4], t4: TC[A5], t5: TC[A6], t6: TC[A7], t7: TC[A8], t8: TC[A9], t9: TC[A10], t10: TC[A11], t11: TC[A12], t12: TC[A13], t13: TC[A14], t14: TC[A15], t15: TC[A16], t16: TC[A17], t17: TC[A18], t18: TC[A19], t19: TC[A20], t20: TC[A21]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]
}

object AndXor21 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
    new AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {}
}
