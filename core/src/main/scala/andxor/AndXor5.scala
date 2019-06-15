package andxor


import andxor.types._
import scalaz.{\/}

trait AndXor5[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor] extends AndXor2[A1, AndXor2[A2, AndXor2[A3, AndXor2[A4, A5]]]] {
  
  def Prod[F[_]](p: (A1#Prod[F], A2#Prod[F], A3#Prod[F], A4#Prod[F], A5#Prod[F])): Prod[F] =
    Prod2[F, A1, AndXor2[A2, AndXor2[A3, AndXor2[A4, A5]]]]((p._1, Prod2[F, A2, AndXor2[A3, AndXor2[A4, A5]]]((p._2, Prod2[F, A3, AndXor2[A4, A5]]((p._3, Prod2[F, A4, A5]((p._4, p._5))))))))

  def Cop[F[_]](c: (A1#Cop[F] \/ (A2#Cop[F] \/ (A3#Cop[F] \/ (A4#Cop[F] \/ A5#Cop[F]))))): Cop[F] =
    Cop2[F, A1, AndXor2[A2, AndXor2[A3, AndXor2[A4, A5]]]](c.map(x1 => Cop2[F, A2, AndXor2[A3, AndXor2[A4, A5]]](x1.map(x2 => Cop2[F, A3, AndXor2[A4, A5]](x2.map(x3 => Cop2[F, A4, A5](x3)))))))
  
}

object AndXor5 {
  def apply[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor]: AndXor5[A1, A2, A3, A4, A5] =
    new AndXor5[A1, A2, A3, A4, A5] {}
}
