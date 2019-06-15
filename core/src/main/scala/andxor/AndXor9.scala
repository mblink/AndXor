package andxor


import andxor.types._
import scalaz.{\/}

trait AndXor9[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor] extends AndXor2[A1, AndXor2[A2, AndXor2[A3, AndXor2[A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, A9]]]]]]]] {
  
  def Prod[F[_]](p: (A1#Prod[F], A2#Prod[F], A3#Prod[F], A4#Prod[F], A5#Prod[F], A6#Prod[F], A7#Prod[F], A8#Prod[F], A9#Prod[F])): Prod[F] =
    Prod2[F, A1, AndXor2[A2, AndXor2[A3, AndXor2[A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, A9]]]]]]]]((p._1, Prod2[F, A2, AndXor2[A3, AndXor2[A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, A9]]]]]]]((p._2, Prod2[F, A3, AndXor2[A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, A9]]]]]]((p._3, Prod2[F, A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, A9]]]]]((p._4, Prod2[F, A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, A9]]]]((p._5, Prod2[F, A6, AndXor2[A7, AndXor2[A8, A9]]]((p._6, Prod2[F, A7, AndXor2[A8, A9]]((p._7, Prod2[F, A8, A9]((p._8, p._9))))))))))))))))

  def Cop[F[_]](c: (A1#Cop[F] \/ (A2#Cop[F] \/ (A3#Cop[F] \/ (A4#Cop[F] \/ (A5#Cop[F] \/ (A6#Cop[F] \/ (A7#Cop[F] \/ (A8#Cop[F] \/ A9#Cop[F]))))))))): Cop[F] =
    Cop2[F, A1, AndXor2[A2, AndXor2[A3, AndXor2[A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, A9]]]]]]]](c.map(x1 => Cop2[F, A2, AndXor2[A3, AndXor2[A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, A9]]]]]]](x1.map(x2 => Cop2[F, A3, AndXor2[A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, A9]]]]]](x2.map(x3 => Cop2[F, A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, A9]]]]](x3.map(x4 => Cop2[F, A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, A9]]]](x4.map(x5 => Cop2[F, A6, AndXor2[A7, AndXor2[A8, A9]]](x5.map(x6 => Cop2[F, A7, AndXor2[A8, A9]](x6.map(x7 => Cop2[F, A8, A9](x7)))))))))))))))
  
}

object AndXor9 {
  def apply[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor]: AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9] =
    new AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9] {}
}
