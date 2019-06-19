package andxor

import andxor.types._
import scalaz.{Apply, Monoid, \/}
import scalaz.Id.Id

trait AndXorNested2[A1[_[_]], A2[_[_]]] extends AndXor {
  type Prod[F[_]] = Prod2[F, A1, A2]
  object Prod {
    def apply[F[_]](p: (A1[F], A2[F])): Prod[F] = Prod2[F, A1, A2](p)
  }

  type Cop[F[_]] = Cop2[F, A1, A2]
  object Cop {
    def apply[F[_]](c: (A1[F] \/ A2[F])): Cop[F] = Cop2[F, A1, A2](c)
  }

  def deriving[TC[_], F[_]](implicit t0: TC[A1[F]], t1: TC[A2[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =
    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose2(t0, t1)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly2(t0, t1)(x => f(Cop2[F, A1, A2](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide2(t0, t1)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply2(t0, t1) {
          case (i0, i1) =>
            f(Prod2[F, A1, A2]((i0, i1)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1[Id]], t1: TC[A2[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] =
      deriving[Inj[Prod[F], ?], F].divide
  }
}

object AndXorNested2 {
  def apply[A1[_[_]], A2[_[_]]]: AndXorNested2[A1, A2] =
    new AndXorNested2[A1, A2] {}
}

trait AndXor2[A1, A2] extends AndXorNested2[FConst[?[_], A1], FConst[?[_], A2]] {
  override def derivingId[TC[_]](implicit t0: TC[A1], t1: TC[A2]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]
}

object AndXor2 {
  def apply[A1, A2]: AndXor2[A1, A2] =
    new AndXor2[A1, A2] {}
}
