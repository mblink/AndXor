package andxor


import andxor.types._
import scalaz.{Monoid}

trait AndXor1[A1] extends AndXor {
  type Prod[F[_]] = Prod1[F, A1]
  object Prod {
    def apply[F[_]](p: F[A1]): Prod[F] =
      Prod1[F, A1](p)
  }

  type Cop[F[_]] = Cop1[F, A1]
  object Cop {
    def apply[F[_]](c: F[A1]): Cop[F] = Cop1[F, A1](c)
  }

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]](implicit d: DerivingCop[Cop, F, Inj[Cop[F], ?]]): Inj[Cop[F], Cop[F]] = choose[Inj[Cop[F], ?], F]
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]], d: DerivingProd[Prod, F, Inj[Prod[F], ?]]): Inj[Prod[F], Prod[F]] = divide[Inj[Prod[F], ?], F]
  }
}

object AndXor1 {
  def apply[A1]: AndXor1[A1] =
    new AndXor1[A1] {}
}
