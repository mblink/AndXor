package andxor


import andxor.types._
import scalaz.{\/}

trait AndXor12[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor] extends AndXor2[A1, AndXor2[A2, AndXor2[A3, AndXor2[A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, AndXor2[A9, AndXor2[A10, AndXor2[A11, A12]]]]]]]]]]] {
  
  def Prod[F[_]](p: (A1#Prod[F], A2#Prod[F], A3#Prod[F], A4#Prod[F], A5#Prod[F], A6#Prod[F], A7#Prod[F], A8#Prod[F], A9#Prod[F], A10#Prod[F], A11#Prod[F], A12#Prod[F])): Prod[F] =
    Prod2[F, A1, AndXor2[A2, AndXor2[A3, AndXor2[A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, AndXor2[A9, AndXor2[A10, AndXor2[A11, A12]]]]]]]]]]]((p._1, Prod2[F, A2, AndXor2[A3, AndXor2[A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, AndXor2[A9, AndXor2[A10, AndXor2[A11, A12]]]]]]]]]]((p._2, Prod2[F, A3, AndXor2[A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, AndXor2[A9, AndXor2[A10, AndXor2[A11, A12]]]]]]]]]((p._3, Prod2[F, A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, AndXor2[A9, AndXor2[A10, AndXor2[A11, A12]]]]]]]]((p._4, Prod2[F, A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, AndXor2[A9, AndXor2[A10, AndXor2[A11, A12]]]]]]]((p._5, Prod2[F, A6, AndXor2[A7, AndXor2[A8, AndXor2[A9, AndXor2[A10, AndXor2[A11, A12]]]]]]((p._6, Prod2[F, A7, AndXor2[A8, AndXor2[A9, AndXor2[A10, AndXor2[A11, A12]]]]]((p._7, Prod2[F, A8, AndXor2[A9, AndXor2[A10, AndXor2[A11, A12]]]]((p._8, Prod2[F, A9, AndXor2[A10, AndXor2[A11, A12]]]((p._9, Prod2[F, A10, AndXor2[A11, A12]]((p._10, Prod2[F, A11, A12]((p._11, p._12))))))))))))))))))))))

  def Cop[F[_]](c: (A1#Cop[F] \/ (A2#Cop[F] \/ (A3#Cop[F] \/ (A4#Cop[F] \/ (A5#Cop[F] \/ (A6#Cop[F] \/ (A7#Cop[F] \/ (A8#Cop[F] \/ (A9#Cop[F] \/ (A10#Cop[F] \/ (A11#Cop[F] \/ A12#Cop[F])))))))))))): Cop[F] =
    Cop2[F, A1, AndXor2[A2, AndXor2[A3, AndXor2[A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, AndXor2[A9, AndXor2[A10, AndXor2[A11, A12]]]]]]]]]]](c.map(x1 => Cop2[F, A2, AndXor2[A3, AndXor2[A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, AndXor2[A9, AndXor2[A10, AndXor2[A11, A12]]]]]]]]]](x1.map(x2 => Cop2[F, A3, AndXor2[A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, AndXor2[A9, AndXor2[A10, AndXor2[A11, A12]]]]]]]]](x2.map(x3 => Cop2[F, A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, AndXor2[A9, AndXor2[A10, AndXor2[A11, A12]]]]]]]](x3.map(x4 => Cop2[F, A5, AndXor2[A6, AndXor2[A7, AndXor2[A8, AndXor2[A9, AndXor2[A10, AndXor2[A11, A12]]]]]]](x4.map(x5 => Cop2[F, A6, AndXor2[A7, AndXor2[A8, AndXor2[A9, AndXor2[A10, AndXor2[A11, A12]]]]]](x5.map(x6 => Cop2[F, A7, AndXor2[A8, AndXor2[A9, AndXor2[A10, AndXor2[A11, A12]]]]](x6.map(x7 => Cop2[F, A8, AndXor2[A9, AndXor2[A10, AndXor2[A11, A12]]]](x7.map(x8 => Cop2[F, A9, AndXor2[A10, AndXor2[A11, A12]]](x8.map(x9 => Cop2[F, A10, AndXor2[A11, A12]](x9.map(x10 => Cop2[F, A11, A12](x10)))))))))))))))))))))
  
}

object AndXor12 {
  def apply[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor]: AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] =
    new AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}
}
