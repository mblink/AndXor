package andxor

import andxor.types._
import andxor.syntax.ffunctor._
import andxor.syntax.ftraverse._
import cats.syntax.either._
import cats.{Applicative, Apply, Functor, Id, MonoidK, ~>}

trait AndXorNested2[A1[_[_]], A2[_[_]]] extends AndXor {

  def *:[B](@annotation.unused a: AndXor1[B]): AndXorNested3[FConst[B]#T, A1, A2] = AndXorNested3[FConst[B]#T, A1, A2]
  def *:[B[_[_]]](@annotation.unused a: AndXorNested1[B]): AndXorNested3[B, A1, A2] = AndXorNested3[B, A1, A2]

  def apply[B1]: AndXorNested3[A1, A2, FConst[B1]#T] = AndXorNested3[A1, A2, FConst[B1]#T]
  def nest[B1[_[_]]]: AndXorNested3[A1, A2, B1] = AndXorNested3[A1, A2, B1]

  def apply[B1, B2]: AndXorNested4[A1, A2, FConst[B1]#T, FConst[B2]#T] = AndXorNested4[A1, A2, FConst[B1]#T, FConst[B2]#T]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested4[A1, A2, B1, B2] = AndXorNested4[A1, A2, B1, B2]

  def apply[B1, B2, B3]: AndXorNested5[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T] = AndXorNested5[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]]]: AndXorNested5[A1, A2, B1, B2, B3] = AndXorNested5[A1, A2, B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXorNested6[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T] = AndXorNested6[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]]]: AndXorNested6[A1, A2, B1, B2, B3, B4] = AndXorNested6[A1, A2, B1, B2, B3, B4]

  def apply[B1, B2, B3, B4, B5]: AndXorNested7[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T] = AndXorNested7[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]]]: AndXorNested7[A1, A2, B1, B2, B3, B4, B5] = AndXorNested7[A1, A2, B1, B2, B3, B4, B5]

  def apply[B1, B2, B3, B4, B5, B6]: AndXorNested8[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T] = AndXorNested8[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]]]: AndXorNested8[A1, A2, B1, B2, B3, B4, B5, B6] = AndXorNested8[A1, A2, B1, B2, B3, B4, B5, B6]

  def apply[B1, B2, B3, B4, B5, B6, B7]: AndXorNested9[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T] = AndXorNested9[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]]]: AndXorNested9[A1, A2, B1, B2, B3, B4, B5, B6, B7] = AndXorNested9[A1, A2, B1, B2, B3, B4, B5, B6, B7]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8]: AndXorNested10[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T] = AndXorNested10[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]]]: AndXorNested10[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8] = AndXorNested10[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9]: AndXorNested11[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T] = AndXorNested11[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]]]: AndXorNested11[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXorNested11[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]: AndXorNested12[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T] = AndXorNested12[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]]]: AndXorNested12[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXorNested12[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]: AndXorNested13[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T] = AndXorNested13[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]]]: AndXorNested13[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] = AndXorNested13[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]: AndXorNested14[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T] = AndXorNested14[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]]]: AndXorNested14[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] = AndXorNested14[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]: AndXorNested15[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T] = AndXorNested15[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]]]: AndXorNested15[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13] = AndXorNested15[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]: AndXorNested16[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T] = AndXorNested16[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]]]: AndXorNested16[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14] = AndXorNested16[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]: AndXorNested17[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T, FConst[B15]#T] = AndXorNested17[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T, FConst[B15]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]]]: AndXorNested17[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15] = AndXorNested17[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]: AndXorNested18[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T, FConst[B15]#T, FConst[B16]#T] = AndXorNested18[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T, FConst[B15]#T, FConst[B16]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]]]: AndXorNested18[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16] = AndXorNested18[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17]: AndXorNested19[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T, FConst[B15]#T, FConst[B16]#T, FConst[B17]#T] = AndXorNested19[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T, FConst[B15]#T, FConst[B16]#T, FConst[B17]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]], B17[_[_]]]: AndXorNested19[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17] = AndXorNested19[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18]: AndXorNested20[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T, FConst[B15]#T, FConst[B16]#T, FConst[B17]#T, FConst[B18]#T] = AndXorNested20[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T, FConst[B15]#T, FConst[B16]#T, FConst[B17]#T, FConst[B18]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]], B17[_[_]], B18[_[_]]]: AndXorNested20[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18] = AndXorNested20[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19]: AndXorNested21[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T, FConst[B15]#T, FConst[B16]#T, FConst[B17]#T, FConst[B18]#T, FConst[B19]#T] = AndXorNested21[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T, FConst[B15]#T, FConst[B16]#T, FConst[B17]#T, FConst[B18]#T, FConst[B19]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]], B17[_[_]], B18[_[_]], B19[_[_]]]: AndXorNested21[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19] = AndXorNested21[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20]: AndXorNested22[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T, FConst[B15]#T, FConst[B16]#T, FConst[B17]#T, FConst[B18]#T, FConst[B19]#T, FConst[B20]#T] = AndXorNested22[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T, FConst[B15]#T, FConst[B16]#T, FConst[B17]#T, FConst[B18]#T, FConst[B19]#T, FConst[B20]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]], B17[_[_]], B18[_[_]], B19[_[_]], B20[_[_]]]: AndXorNested22[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20] = AndXorNested22[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20]

  type Prod[F[_]] = Prod2[Id, A1[F], A2[F]]
  object Prod {
    def apply[F[_]](p: (A1[F], A2[F])): Prod[F] = Prod2[Id, A1[F], A2[F]](p)
  }

  type Cop[F[_]] = Cop2[Id, A1[F], A2[F]]
  object Cop {
    def apply[F[_]](c: Either[A1[F], A2[F]]): Cop[F] = Cop2[Id, A1[F], A2[F]](c)
  }

  object instances {
    implicit def axoProd2Instance(implicit ft0: FTraverse[A1, Applicative], ft1: FTraverse[A2, Applicative]): FFunctor[Prod] with FTraverseProd[Prod] =
      new FFunctor[Prod] with FTraverseProd[Prod] {
        def map[F[_], G[_]](p: Prod2[Id, A1[F], A2[F]])(nt: F ~> G): Prod2[Id, A1[G], A2[G]] =
          Prod2[Id, A1[G], A2[G]]((ft0.map(p.t1)(nt), ft1.map(p.t2)(nt)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod2[Id, A1[F], A2[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Prod2[Id, A1[G], A2[G]]] =
          Applicative[A].ap(Applicative[A].map(ft0.traverse(p.t1)(f))((i0: A1[G]) => (i1: A2[G]) => Prod2[Id, A1[G], A2[G]]((i0, i1))))(ft1.traverse(p.t2)(f))
      }

    implicit def axoProd2FoldMap(implicit fm0: FoldMap[A1, A1], fm1: FoldMap[A2, A2]): FoldMap[Prod, Cop] =
      new FoldMap[Prod, Cop] {
        def emptyProd[F[_]](implicit PE: MonoidK[F]): Prod[F] =
          Prod((fm0.emptyProd, fm1.emptyProd))

        def unconsAll[F[_], G[_]](p: Prod2[Id, A1[F], A2[F]])(implicit U: Uncons[F, G]): (List[Cop2[Id, A1[G], A2[G]]], Prod2[Id, A1[F], A2[F]]) = {
          val (h1, t1) = fm0.unconsAll(p.t1)
          val (h2, t2) = fm1.unconsAll(p.t2)
          (
            List(h1.map(Inj[Cop2[Id, A1[G], A2[G]], A1[G]].apply(_)), h2.map(Inj[Cop2[Id, A1[G], A2[G]], A2[G]].apply(_))).flatten,
            Prod2[Id, A1[F], A2[F]]((t1, t2)))
        }

        def unconsOne[F[_], G[_]](p: Prod2[Id, A1[F], A2[F]], c: Cop2[Id, A1[G], A2[G]])(implicit U: Uncons[F, G]): (Option[Cop2[Id, A1[G], A2[G]]], Prod2[Id, A1[F], A2[F]]) =
          c.run match {

            case Left(x) =>
              val (h, t) = fm0.unconsOne(p.t1, x)
              (h.map(v => Cop2[Id, A1[G], A2[G]](Left(v))), Prod2[Id, A1[F], A2[F]]((t, p.t2)))

            case Right(x) =>
              val (h, t) = fm1.unconsOne(p.t2, x)
              (h.map(v => Cop2[Id, A1[G], A2[G]](Right(v))), Prod2[Id, A1[F], A2[F]]((p.t1, t)))

          }
      }

    implicit def axoCop2Instance(implicit ft0: FTraverse[A1, Functor], ft1: FTraverse[A2, Functor]): FFunctor[Cop] with FTraverseCop[Cop] =
      new FFunctor[Cop] with FTraverseCop[Cop] {
        def map[F[_], G[_]](c: Cop2[Id, A1[F], A2[F]])(nt: F ~> G): Cop2[Id, A1[G], A2[G]] =
          Cop2[Id, A1[G], A2[G]](c.run.bimap(_.map(nt), _.map(nt)))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop2[Id, A1[F], A2[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Cop2[Id, A1[G], A2[G]]] =
          c.run match {

            case Left(x) => Functor[A].map(x.traverse(f))(y => Cop2[Id, A1[G], A2[G]](Left(y)))

            case Right(x) => Functor[A].map(x.traverse(f))(y => Cop2[Id, A1[G], A2[G]](Right(y)))

          }
      }
  }

  def deriving[TC[_], F[_]](implicit t0: TC[A1[F]], t1: TC[A2[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose2(t0, t1)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly2(t0, t1)(x => f(Cop2[Id, A1[F], A2[F]](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide2(t0, t1)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply2(t0, t1) {
          case (i0, i1) =>
            f(Prod2[Id, A1[F], A2[F]]((i0, i1)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1[Id]], t1: TC[A2[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]
}

object AndXorNested2 {
  def apply[A1[_[_]], A2[_[_]]]: AndXorNested2[A1, A2] =
    new AndXorNested2[A1, A2] {}
}

trait AndXor2[A1, A2] extends AndXor {

  def *:[B](@annotation.unused a: AndXor1[B]): AndXor3[B, A1, A2] = AndXor3[B, A1, A2]
  def *:[B[_[_]]](@annotation.unused a: AndXorNested1[B]): AndXorNested3[B, FConst[A1]#T, FConst[A2]#T] = AndXorNested3[B, FConst[A1]#T, FConst[A2]#T]

  def apply[B1]: AndXor3[A1, A2, B1] = AndXor3[A1, A2, B1]
  def nest[B1[_[_]]]: AndXorNested3[FConst[A1]#T, FConst[A2]#T, B1] = AndXorNested3[FConst[A1]#T, FConst[A2]#T, B1]

  def apply[B1, B2]: AndXor4[A1, A2, B1, B2] = AndXor4[A1, A2, B1, B2]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested4[FConst[A1]#T, FConst[A2]#T, B1, B2] = AndXorNested4[FConst[A1]#T, FConst[A2]#T, B1, B2]

  def apply[B1, B2, B3]: AndXor5[A1, A2, B1, B2, B3] = AndXor5[A1, A2, B1, B2, B3]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]]]: AndXorNested5[FConst[A1]#T, FConst[A2]#T, B1, B2, B3] = AndXorNested5[FConst[A1]#T, FConst[A2]#T, B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXor6[A1, A2, B1, B2, B3, B4] = AndXor6[A1, A2, B1, B2, B3, B4]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]]]: AndXorNested6[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4] = AndXorNested6[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4]

  def apply[B1, B2, B3, B4, B5]: AndXor7[A1, A2, B1, B2, B3, B4, B5] = AndXor7[A1, A2, B1, B2, B3, B4, B5]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]]]: AndXorNested7[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5] = AndXorNested7[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5]

  def apply[B1, B2, B3, B4, B5, B6]: AndXor8[A1, A2, B1, B2, B3, B4, B5, B6] = AndXor8[A1, A2, B1, B2, B3, B4, B5, B6]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]]]: AndXorNested8[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6] = AndXorNested8[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6]

  def apply[B1, B2, B3, B4, B5, B6, B7]: AndXor9[A1, A2, B1, B2, B3, B4, B5, B6, B7] = AndXor9[A1, A2, B1, B2, B3, B4, B5, B6, B7]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]]]: AndXorNested9[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7] = AndXorNested9[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8]: AndXor10[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8] = AndXor10[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]]]: AndXorNested10[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8] = AndXorNested10[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9]: AndXor11[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXor11[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]]]: AndXorNested11[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXorNested11[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]: AndXor12[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXor12[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]]]: AndXorNested12[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXorNested12[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]: AndXor13[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] = AndXor13[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]]]: AndXorNested13[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] = AndXorNested13[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]: AndXor14[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] = AndXor14[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]]]: AndXorNested14[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] = AndXorNested14[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]: AndXor15[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13] = AndXor15[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]]]: AndXorNested15[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13] = AndXorNested15[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]: AndXor16[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14] = AndXor16[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]]]: AndXorNested16[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14] = AndXorNested16[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]: AndXor17[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15] = AndXor17[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]]]: AndXorNested17[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15] = AndXorNested17[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]: AndXor18[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16] = AndXor18[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]]]: AndXorNested18[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16] = AndXorNested18[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17]: AndXor19[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17] = AndXor19[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]], B17[_[_]]]: AndXorNested19[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17] = AndXorNested19[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18]: AndXor20[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18] = AndXor20[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]], B17[_[_]], B18[_[_]]]: AndXorNested20[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18] = AndXorNested20[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19]: AndXor21[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19] = AndXor21[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]], B17[_[_]], B18[_[_]], B19[_[_]]]: AndXorNested21[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19] = AndXorNested21[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20]: AndXor22[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20] = AndXor22[A1, A2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]], B17[_[_]], B18[_[_]], B19[_[_]], B20[_[_]]]: AndXorNested22[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20] = AndXorNested22[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20]

  type Prod[F[_]] = Prod2[F, A1, A2]
  object Prod {
    def apply[F[_]](p: (F[A1], F[A2])): Prod[F] = Prod2[F, A1, A2](p)
  }

  type Cop[F[_]] = Cop2[F, A1, A2]
  object Cop {
    def apply[F[_]](c: Either[F[A1], F[A2]]): Cop[F] = Cop2[F, A1, A2](c)
  }

  def deriving[TC[_], F[_]](implicit t0: TC[F[A1]], t1: TC[F[A2]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose2(t0, t1)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly2(t0, t1)(x => f(Cop2[F, A1, A2](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide2(t0, t1)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply2(t0, t1) {
          case (i0, i1) =>
            f(Prod2[F, A1, A2]((i0, i1)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1], t1: TC[A2]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]
}

object AndXor2 {
  def apply[A1, A2]: AndXor2[A1, A2] =
    new AndXor2[A1, A2] {}
}
