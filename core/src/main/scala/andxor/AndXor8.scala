package andxor


import andxor.types._
import scalaz.{\/}

trait AndXor8[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor] extends AndXor2[A1, AndXor2[A2, AndXor2[A3, AndXor2[A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, A8]]]]]]] {
  
  def Prod[F[_]](p: (A1#Prod[F], A2#Prod[F], A3#Prod[F], A4#Prod[F], A5#Prod[F], A6#Prod[F], A7#Prod[F], A8#Prod[F])): Prod[F] =
    Prod2[F, A1, AndXor2[A2, AndXor2[A3, AndXor2[A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, A8]]]]]]]((p._1, Prod2[F, A2, AndXor2[A3, AndXor2[A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, A8]]]]]]((p._2, Prod2[F, A3, AndXor2[A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, A8]]]]]((p._3, Prod2[F, A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, A8]]]]((p._4, Prod2[F, A5, AndXor2[A6, AndXor2[A7, A8]]]((p._5, Prod2[F, A6, AndXor2[A7, A8]]((p._6, Prod2[F, A7, A8]((p._7, p._8))))))))))))))

  def Cop[F[_]](c: (A1#Cop[F] \/ (A2#Cop[F] \/ (A3#Cop[F] \/ (A4#Cop[F] \/ (A5#Cop[F] \/ (A6#Cop[F] \/ (A7#Cop[F] \/ A8#Cop[F])))))))): Cop[F] =
    Cop2[F, A1, AndXor2[A2, AndXor2[A3, AndXor2[A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, A8]]]]]]](c.map(x1 => Cop2[F, A2, AndXor2[A3, AndXor2[A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, A8]]]]]](x1.map(x2 => Cop2[F, A3, AndXor2[A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, A8]]]]](x2.map(x3 => Cop2[F, A4, AndXor2[A5, AndXor2[A6, AndXor2[A7, A8]]]](x3.map(x4 => Cop2[F, A5, AndXor2[A6, AndXor2[A7, A8]]](x4.map(x5 => Cop2[F, A6, AndXor2[A7, A8]](x5.map(x6 => Cop2[F, A7, A8](x6)))))))))))))
  
}

object AndXor8 {
  def apply[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor]: AndXor8[A1, A2, A3, A4, A5, A6, A7, A8] =
    new AndXor8[A1, A2, A3, A4, A5, A6, A7, A8] {}
}
