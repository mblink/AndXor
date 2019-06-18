package andxor


import andxor.types._
import scalaz.{Monoid, \/}

trait AndXor2[A1[_[_]], A2[_[_]]] extends AndXor {
  type Prod[F[_]] = Prod2[F, A1, A2]
  object Prod {
    def apply[F[_]](p: (A1[F], A2[F])): Prod[F] = Prod2[F, A1, A2](p)
  }

  type Cop[F[_]] = Cop2[F, A1, A2]
  object Cop {
    def apply[F[_]](c: (A1[F] \/ A2[F])): Cop[F] = Cop2[F, A1, A2](c)
  }

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]](implicit d: DerivingCop[Cop, F, Inj[Cop[F], ?]]): Inj[Cop[F], Cop[F]] = choose[Inj[Cop[F], ?], F]
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]], d: DerivingProd[Prod, F, Inj[Prod[F], ?]]): Inj[Prod[F], Prod[F]] = divide[Inj[Prod[F], ?], F]
  }
}

object AndXor2 {
  def apply[A1[_[_]], A2[_[_]]]: AndXor2[A1, A2] =
    new AndXor2[A1, A2] {}
}
