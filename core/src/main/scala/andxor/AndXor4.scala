package andxor


import andxor.types._
import scalaz.{Monoid, \/}

trait AndXor4[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor] extends AndXor {
  type Prod[F[_]] = Prod4[F, A1, A2, A3, A4]
  object Prod {
    def apply[F[_]](p: (A1#Prod[F], A2#Prod[F], A3#Prod[F], A4#Prod[F])): Prod[F] = Prod4[F, A1, A2, A3, A4](p)
  }

  type Cop[F[_]] = Cop4[F, A1, A2, A3, A4]
  object Cop {
    def apply[F[_]](c: (A1#Cop[F] \/ (A2#Cop[F] \/ (A3#Cop[F] \/ A4#Cop[F])))): Cop[F] = Cop4[F, A1, A2, A3, A4](c)
  }

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]](implicit d: DerivingCop[Cop, F, Inj[Cop[F], ?]]): Inj[Cop[F], Cop[F]] = choose[Inj[Cop[F], ?], F]
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]], d: DerivingProd[Prod, F, Inj[Prod[F], ?]]): Inj[Prod[F], Prod[F]] = divide[Inj[Prod[F], ?], F]
  }
}

object AndXor4 {
  def apply[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor]: AndXor4[A1, A2, A3, A4] =
    new AndXor4[A1, A2, A3, A4] {}
}
