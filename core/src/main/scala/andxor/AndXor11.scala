package andxor

import andxor.types._
import scalaz.{Apply, Monoid, \/}
import scalaz.Id.Id

trait AndXorNested11[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]]] extends AndXor {
  type Prod[F[_]] = Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]
  object Prod {
    def apply[F[_]](p: (A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F])): Prod[F] = Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](p)
  }

  type Cop[F[_]] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]
  object Cop {
    def apply[F[_]](c: (A1[F] \/ (A2[F] \/ (A3[F] \/ (A4[F] \/ (A5[F] \/ (A6[F] \/ (A7[F] \/ (A8[F] \/ (A9[F] \/ (A10[F] \/ A11[F]))))))))))): Cop[F] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](c)
  }

  def deriving[TC[_], F[_]](implicit t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =
    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose11(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly11(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)(x => f(Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide11(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply11(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) {
          case (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10) =>
            f(Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1[Id]], t1: TC[A2[Id]], t2: TC[A3[Id]], t3: TC[A4[Id]], t4: TC[A5[Id]], t5: TC[A6[Id]], t6: TC[A7[Id]], t7: TC[A8[Id]], t8: TC[A9[Id]], t9: TC[A10[Id]], t10: TC[A11[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] =
      deriving[Inj[Prod[F], ?], F].divide
  }
}

object AndXorNested11 {
  def apply[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]]]: AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] =
    new AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {}
}

trait AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] extends AndXorNested11[AndXorConst[A1]#AXO, AndXorConst[A2]#AXO, AndXorConst[A3]#AXO, AndXorConst[A4]#AXO, AndXorConst[A5]#AXO, AndXorConst[A6]#AXO, AndXorConst[A7]#AXO, AndXorConst[A8]#AXO, AndXorConst[A9]#AXO, AndXorConst[A10]#AXO, AndXorConst[A11]#AXO] {
  override def derivingId[TC[_]](implicit t0: TC[A1], t1: TC[A2], t2: TC[A3], t3: TC[A4], t4: TC[A5], t5: TC[A6], t6: TC[A7], t7: TC[A8], t8: TC[A9], t9: TC[A10], t10: TC[A11]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]
}

object AndXor11 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] =
    new AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {}
}
