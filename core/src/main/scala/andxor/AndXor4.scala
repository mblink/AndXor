package andxor


import andxor.types._
import scalaz.{\/}

trait AndXor4[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor] extends AndXor2[A1, AndXor2[A2, AndXor2[A3, A4]]] {
  
  def Prod[F[_]](p: (A1#Prod[F], A2#Prod[F], A3#Prod[F], A4#Prod[F])): Prod[F] =
    Prod2[F, A1, AndXor2[A2, AndXor2[A3, A4]]]((p._1, Prod2[F, A2, AndXor2[A3, A4]]((p._2, Prod2[F, A3, A4]((p._3, p._4))))))

  def Cop[F[_]](c: (A1#Cop[F] \/ (A2#Cop[F] \/ (A3#Cop[F] \/ A4#Cop[F])))): Cop[F] =
    Cop2[F, A1, AndXor2[A2, AndXor2[A3, A4]]](c.map(x1 => Cop2[F, A2, AndXor2[A3, A4]](x1.map(x2 => Cop2[F, A3, A4](x2)))))
  
}

object AndXor4 {
  def apply[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor]: AndXor4[A1, A2, A3, A4] =
    new AndXor4[A1, A2, A3, A4] {}
}
