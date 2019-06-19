package andxor

import andxor.types._
import scalaz.{Apply, Monoid, \/}
import scalaz.Id.Id

trait AndXorNested8[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]]] extends AndXor {
  type Prod[F[_]] = Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]
  object Prod {
    def apply[F[_]](p: (A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F])): Prod[F] = Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8](p)
  }

  type Cop[F[_]] = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]
  object Cop {
    def apply[F[_]](c: (A1[F] \/ (A2[F] \/ (A3[F] \/ (A4[F] \/ (A5[F] \/ (A6[F] \/ (A7[F] \/ A8[F])))))))): Cop[F] = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](c)
  }

  def deriving[TC[_], F[_]](implicit t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =
    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose8(t0, t1, t2, t3, t4, t5, t6, t7)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly8(t0, t1, t2, t3, t4, t5, t6, t7)(x => f(Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide8(t0, t1, t2, t3, t4, t5, t6, t7)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply8(t0, t1, t2, t3, t4, t5, t6, t7) {
          case (i0, i1, i2, i3, i4, i5, i6, i7) =>
            f(Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((i0, i1, i2, i3, i4, i5, i6, i7)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1[Id]], t1: TC[A2[Id]], t2: TC[A3[Id]], t3: TC[A4[Id]], t4: TC[A5[Id]], t5: TC[A6[Id]], t6: TC[A7[Id]], t7: TC[A8[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] =
      deriving[Inj[Prod[F], ?], F].divide
  }
}

object AndXorNested8 {
  def apply[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]]]: AndXorNested8[A1, A2, A3, A4, A5, A6, A7, A8] =
    new AndXorNested8[A1, A2, A3, A4, A5, A6, A7, A8] {}
}

trait AndXor8[A1, A2, A3, A4, A5, A6, A7, A8] extends AndXorNested8[FConst[?[_], A1], FConst[?[_], A2], FConst[?[_], A3], FConst[?[_], A4], FConst[?[_], A5], FConst[?[_], A6], FConst[?[_], A7], FConst[?[_], A8]] {
  override def derivingId[TC[_]](implicit t0: TC[A1], t1: TC[A2], t2: TC[A3], t3: TC[A4], t4: TC[A5], t5: TC[A6], t6: TC[A7], t7: TC[A8]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]
}

object AndXor8 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8]: AndXor8[A1, A2, A3, A4, A5, A6, A7, A8] =
    new AndXor8[A1, A2, A3, A4, A5, A6, A7, A8] {}
}
