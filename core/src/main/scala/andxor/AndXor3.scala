package andxor


import andxor.types._
import scalaz.{Monoid, \/}

trait AndXor3[A1[_[_]], A2[_[_]], A3[_[_]]] extends AndXor {
  type Prod[F[_]] = Prod3[F, A1, A2, A3]
  object Prod {
    def apply[F[_]](p: (A1[F], A2[F], A3[F])): Prod[F] = Prod3[F, A1, A2, A3](p)
  }

  type Cop[F[_]] = Cop3[F, A1, A2, A3]
  object Cop {
    def apply[F[_]](c: (A1[F] \/ (A2[F] \/ A3[F]))): Cop[F] = Cop3[F, A1, A2, A3](c)
  }

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]](implicit d: DerivingCop[Cop, F, Inj[Cop[F], ?]]): Inj[Cop[F], Cop[F]] = choose[Inj[Cop[F], ?], F]
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]], d: DerivingProd[Prod, F, Inj[Prod[F], ?]]): Inj[Prod[F], Prod[F]] = divide[Inj[Prod[F], ?], F]
  }
}

object AndXor3 {
  def apply[A1[_[_]], A2[_[_]], A3[_[_]]]: AndXor3[A1, A2, A3] =
    new AndXor3[A1, A2, A3] {}
}
