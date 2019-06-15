package andxor


import andxor.types._
import scalaz.{Monoid, \/}

trait AndXor10[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor] extends AndXor {
  type Prod[F[_]] = Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]
  object Prod {
    def apply[F[_]](p: (A1#Prod[F], A2#Prod[F], A3#Prod[F], A4#Prod[F], A5#Prod[F], A6#Prod[F], A7#Prod[F], A8#Prod[F], A9#Prod[F], A10#Prod[F])): Prod[F] = Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](p)
  }

  type Cop[F[_]] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]
  object Cop {
    def apply[F[_]](c: (A1#Cop[F] \/ (A2#Cop[F] \/ (A3#Cop[F] \/ (A4#Cop[F] \/ (A5#Cop[F] \/ (A6#Cop[F] \/ (A7#Cop[F] \/ (A8#Cop[F] \/ (A9#Cop[F] \/ A10#Cop[F])))))))))): Cop[F] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](c)
  }

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]](implicit d: DerivingCop[Cop, F, Inj[Cop[F], ?]]): Inj[Cop[F], Cop[F]] = choose[Inj[Cop[F], ?], F]
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]], d: DerivingProd[Prod, F, Inj[Prod[F], ?]]): Inj[Prod[F], Prod[F]] = divide[Inj[Prod[F], ?], F]
  }
}

object AndXor10 {
  def apply[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor]: AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] =
    new AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {}
}
