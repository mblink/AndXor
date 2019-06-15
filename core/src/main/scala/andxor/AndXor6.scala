package andxor


import andxor.types._
import scalaz.{Monoid, \/}

trait AndXor6[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor] extends AndXor {
  type Prod[F[_]] = Prod6[F, A1, A2, A3, A4, A5, A6]
  object Prod {
    def apply[F[_]](p: (A1#Prod[F], A2#Prod[F], A3#Prod[F], A4#Prod[F], A5#Prod[F], A6#Prod[F])): Prod[F] = Prod6[F, A1, A2, A3, A4, A5, A6](p)
  }

  type Cop[F[_]] = Cop6[F, A1, A2, A3, A4, A5, A6]
  object Cop {
    def apply[F[_]](c: (A1#Cop[F] \/ (A2#Cop[F] \/ (A3#Cop[F] \/ (A4#Cop[F] \/ (A5#Cop[F] \/ A6#Cop[F])))))): Cop[F] = Cop6[F, A1, A2, A3, A4, A5, A6](c)
  }

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]](implicit d: DerivingCop[Cop, F, Inj[Cop[F], ?]]): Inj[Cop[F], Cop[F]] = choose[Inj[Cop[F], ?], F]
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]], d: DerivingProd[Prod, F, Inj[Prod[F], ?]]): Inj[Prod[F], Prod[F]] = divide[Inj[Prod[F], ?], F]
  }
}

object AndXor6 {
  def apply[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor]: AndXor6[A1, A2, A3, A4, A5, A6] =
    new AndXor6[A1, A2, A3, A4, A5, A6] {}
}
