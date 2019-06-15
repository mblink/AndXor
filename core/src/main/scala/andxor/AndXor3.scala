package andxor


import andxor.types._
import scalaz.{\/}

trait AndXor3[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor] extends AndXor2[A1, AndXor2[A2, A3]] {
  
  def Prod[F[_]](p: (A1#Prod[F], A2#Prod[F], A3#Prod[F])): Prod[F] =
    Prod2[F, A1, AndXor2[A2, A3]]((p._1, Prod2[F, A2, A3]((p._2, p._3))))

  def Cop[F[_]](c: (A1#Cop[F] \/ (A2#Cop[F] \/ A3#Cop[F]))): Cop[F] =
    Cop2[F, A1, AndXor2[A2, A3]](c.map(x1 => Cop2[F, A2, A3](x1)))
  
}

object AndXor3 {
  def apply[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor]: AndXor3[A1, A2, A3] =
    new AndXor3[A1, A2, A3] {}
}
