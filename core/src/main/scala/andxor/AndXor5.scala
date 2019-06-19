package andxor

import andxor.types._
import scalaz.{Apply, Monoid, \/}
import scalaz.Id.Id

trait AndXorNested5[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]]] extends AndXor {
  type Prod[F[_]] = Prod5[F, A1, A2, A3, A4, A5]
  object Prod {
    def apply[F[_]](p: (A1[F], A2[F], A3[F], A4[F], A5[F])): Prod[F] = Prod5[F, A1, A2, A3, A4, A5](p)
  }

  type Cop[F[_]] = Cop5[F, A1, A2, A3, A4, A5]
  object Cop {
    def apply[F[_]](c: (A1[F] \/ (A2[F] \/ (A3[F] \/ (A4[F] \/ A5[F]))))): Cop[F] = Cop5[F, A1, A2, A3, A4, A5](c)
  }

  def deriving[TC[_], F[_]](implicit t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =
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

  def derivingId[TC[_]](implicit t0: TC[A1[Id]], t1: TC[A2[Id]], t2: TC[A3[Id]], t3: TC[A4[Id]], t4: TC[A5[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] =
      deriving[Inj[Prod[F], ?], F].divide
  }
}

object AndXorNested5 {
  def apply[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]]]: AndXorNested5[A1, A2, A3, A4, A5] =
    new AndXorNested5[A1, A2, A3, A4, A5] {}
}

trait AndXor5[A1, A2, A3, A4, A5] extends AndXorNested5[AndXorConst[A1]#AXO, AndXorConst[A2]#AXO, AndXorConst[A3]#AXO, AndXorConst[A4]#AXO, AndXorConst[A5]#AXO] {
  override def derivingId[TC[_]](implicit t0: TC[A1], t1: TC[A2], t2: TC[A3], t3: TC[A4], t4: TC[A5]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]
}

object AndXor5 {
  def apply[A1, A2, A3, A4, A5]: AndXor5[A1, A2, A3, A4, A5] =
    new AndXor5[A1, A2, A3, A4, A5] {}
}
