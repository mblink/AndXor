package andxor

import andxor.types._

import cats.{Apply, Id}

trait AndXorNested1[A1[_[_]]] extends AndXor {

  def *:[B](@annotation.unused a: AndXor1[B]): AndXorNested2[Lambda[f[_] => f[B]], A1] = AndXorNested2[Lambda[f[_] => f[B]], A1]
  def *:[B[_[_]]](@annotation.unused a: AndXorNested1[B]): AndXorNested2[B, A1] = AndXorNested2[B, A1]

  def apply[B1]: AndXorNested2[A1, Lambda[f[_] => f[B1]]] = AndXorNested2[A1, Lambda[f[_] => f[B1]]]
  def nest[B1[_[_]]]: AndXorNested2[A1, B1] = AndXorNested2[A1, B1]

  def apply[B1, B2]: AndXorNested3[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]]] = AndXorNested3[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]]]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested3[A1, B1, B2] = AndXorNested3[A1, B1, B2]

  def apply[B1, B2, B3]: AndXorNested4[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]]] = AndXorNested4[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]]]: AndXorNested4[A1, B1, B2, B3] = AndXorNested4[A1, B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXorNested5[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]]] = AndXorNested5[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]]]: AndXorNested5[A1, B1, B2, B3, B4] = AndXorNested5[A1, B1, B2, B3, B4]

  def apply[B1, B2, B3, B4, B5]: AndXorNested6[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]]] = AndXorNested6[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]]]: AndXorNested6[A1, B1, B2, B3, B4, B5] = AndXorNested6[A1, B1, B2, B3, B4, B5]

  def apply[B1, B2, B3, B4, B5, B6]: AndXorNested7[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]]] = AndXorNested7[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]]]: AndXorNested7[A1, B1, B2, B3, B4, B5, B6] = AndXorNested7[A1, B1, B2, B3, B4, B5, B6]

  def apply[B1, B2, B3, B4, B5, B6, B7]: AndXorNested8[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]]] = AndXorNested8[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]]]: AndXorNested8[A1, B1, B2, B3, B4, B5, B6, B7] = AndXorNested8[A1, B1, B2, B3, B4, B5, B6, B7]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8]: AndXorNested9[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]]] = AndXorNested9[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]]]: AndXorNested9[A1, B1, B2, B3, B4, B5, B6, B7, B8] = AndXorNested9[A1, B1, B2, B3, B4, B5, B6, B7, B8]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9]: AndXorNested10[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]]] = AndXorNested10[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]]]: AndXorNested10[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXorNested10[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]: AndXorNested11[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]]] = AndXorNested11[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]]]: AndXorNested11[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXorNested11[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]: AndXorNested12[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]]] = AndXorNested12[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]]]: AndXorNested12[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] = AndXorNested12[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]: AndXorNested13[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]]] = AndXorNested13[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]]]: AndXorNested13[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] = AndXorNested13[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]: AndXorNested14[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]]] = AndXorNested14[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]]]: AndXorNested14[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13] = AndXorNested14[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]: AndXorNested15[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]]] = AndXorNested15[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]]]: AndXorNested15[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14] = AndXorNested15[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]: AndXorNested16[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]], Lambda[f[_] => f[B15]]] = AndXorNested16[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]], Lambda[f[_] => f[B15]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]]]: AndXorNested16[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15] = AndXorNested16[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]: AndXorNested17[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]], Lambda[f[_] => f[B15]], Lambda[f[_] => f[B16]]] = AndXorNested17[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]], Lambda[f[_] => f[B15]], Lambda[f[_] => f[B16]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]]]: AndXorNested17[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16] = AndXorNested17[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17]: AndXorNested18[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]], Lambda[f[_] => f[B15]], Lambda[f[_] => f[B16]], Lambda[f[_] => f[B17]]] = AndXorNested18[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]], Lambda[f[_] => f[B15]], Lambda[f[_] => f[B16]], Lambda[f[_] => f[B17]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]], B17[_[_]]]: AndXorNested18[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17] = AndXorNested18[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18]: AndXorNested19[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]], Lambda[f[_] => f[B15]], Lambda[f[_] => f[B16]], Lambda[f[_] => f[B17]], Lambda[f[_] => f[B18]]] = AndXorNested19[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]], Lambda[f[_] => f[B15]], Lambda[f[_] => f[B16]], Lambda[f[_] => f[B17]], Lambda[f[_] => f[B18]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]], B17[_[_]], B18[_[_]]]: AndXorNested19[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18] = AndXorNested19[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19]: AndXorNested20[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]], Lambda[f[_] => f[B15]], Lambda[f[_] => f[B16]], Lambda[f[_] => f[B17]], Lambda[f[_] => f[B18]], Lambda[f[_] => f[B19]]] = AndXorNested20[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]], Lambda[f[_] => f[B15]], Lambda[f[_] => f[B16]], Lambda[f[_] => f[B17]], Lambda[f[_] => f[B18]], Lambda[f[_] => f[B19]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]], B17[_[_]], B18[_[_]], B19[_[_]]]: AndXorNested20[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19] = AndXorNested20[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20]: AndXorNested21[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]], Lambda[f[_] => f[B15]], Lambda[f[_] => f[B16]], Lambda[f[_] => f[B17]], Lambda[f[_] => f[B18]], Lambda[f[_] => f[B19]], Lambda[f[_] => f[B20]]] = AndXorNested21[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]], Lambda[f[_] => f[B15]], Lambda[f[_] => f[B16]], Lambda[f[_] => f[B17]], Lambda[f[_] => f[B18]], Lambda[f[_] => f[B19]], Lambda[f[_] => f[B20]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]], B17[_[_]], B18[_[_]], B19[_[_]], B20[_[_]]]: AndXorNested21[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20] = AndXorNested21[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21]: AndXorNested22[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]], Lambda[f[_] => f[B15]], Lambda[f[_] => f[B16]], Lambda[f[_] => f[B17]], Lambda[f[_] => f[B18]], Lambda[f[_] => f[B19]], Lambda[f[_] => f[B20]], Lambda[f[_] => f[B21]]] = AndXorNested22[A1, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]], Lambda[f[_] => f[B15]], Lambda[f[_] => f[B16]], Lambda[f[_] => f[B17]], Lambda[f[_] => f[B18]], Lambda[f[_] => f[B19]], Lambda[f[_] => f[B20]], Lambda[f[_] => f[B21]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]], B17[_[_]], B18[_[_]], B19[_[_]], B20[_[_]], B21[_[_]]]: AndXorNested22[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21] = AndXorNested22[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21]

  type Prod[F[_]] = Prod1[Id, A1[F]]
  object Prod {
    def apply[F[_]](p: A1[F]): Prod[F] = p
  }

  type Cop[F[_]] = Cop1[Id, A1[F]]
  object Cop {
    def apply[F[_]](c: A1[F]): Cop[F] = c
  }

  def deriving[TC[_], F[_]](implicit t0: TC[A1[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        d.contramap(t0)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        a.map(t0)(x => f(Cop1[Id, A1[F]](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        a.contramap(t0)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =
        a.map(t0)(x => f(Prod1[Id, A1[F]](x)))
    }

  def derivingId[TC[_]](implicit t0: TC[A1[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]
}

object AndXorNested1 {
  def apply[A1[_[_]]]: AndXorNested1[A1] =
    new AndXorNested1[A1] {}
}

trait AndXor1[A1] extends AndXor {

  def *:[B](@annotation.unused a: AndXor1[B]): AndXor2[B, A1] = AndXor2[B, A1]
  def *:[B[_[_]]](@annotation.unused a: AndXorNested1[B]): AndXorNested2[B, Lambda[f[_] => f[A1]]] = AndXorNested2[B, Lambda[f[_] => f[A1]]]

  def apply[B1]: AndXor2[A1, B1] = AndXor2[A1, B1]
  def nest[B1[_[_]]]: AndXorNested2[Lambda[f[_] => f[A1]], B1] = AndXorNested2[Lambda[f[_] => f[A1]], B1]

  def apply[B1, B2]: AndXor3[A1, B1, B2] = AndXor3[A1, B1, B2]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested3[Lambda[f[_] => f[A1]], B1, B2] = AndXorNested3[Lambda[f[_] => f[A1]], B1, B2]

  def apply[B1, B2, B3]: AndXor4[A1, B1, B2, B3] = AndXor4[A1, B1, B2, B3]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]]]: AndXorNested4[Lambda[f[_] => f[A1]], B1, B2, B3] = AndXorNested4[Lambda[f[_] => f[A1]], B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXor5[A1, B1, B2, B3, B4] = AndXor5[A1, B1, B2, B3, B4]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]]]: AndXorNested5[Lambda[f[_] => f[A1]], B1, B2, B3, B4] = AndXorNested5[Lambda[f[_] => f[A1]], B1, B2, B3, B4]

  def apply[B1, B2, B3, B4, B5]: AndXor6[A1, B1, B2, B3, B4, B5] = AndXor6[A1, B1, B2, B3, B4, B5]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]]]: AndXorNested6[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5] = AndXorNested6[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5]

  def apply[B1, B2, B3, B4, B5, B6]: AndXor7[A1, B1, B2, B3, B4, B5, B6] = AndXor7[A1, B1, B2, B3, B4, B5, B6]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]]]: AndXorNested7[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6] = AndXorNested7[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6]

  def apply[B1, B2, B3, B4, B5, B6, B7]: AndXor8[A1, B1, B2, B3, B4, B5, B6, B7] = AndXor8[A1, B1, B2, B3, B4, B5, B6, B7]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]]]: AndXorNested8[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7] = AndXorNested8[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8]: AndXor9[A1, B1, B2, B3, B4, B5, B6, B7, B8] = AndXor9[A1, B1, B2, B3, B4, B5, B6, B7, B8]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]]]: AndXorNested9[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8] = AndXorNested9[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9]: AndXor10[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXor10[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]]]: AndXorNested10[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXorNested10[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]: AndXor11[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXor11[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]]]: AndXorNested11[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXorNested11[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]: AndXor12[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] = AndXor12[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]]]: AndXorNested12[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] = AndXorNested12[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]: AndXor13[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] = AndXor13[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]]]: AndXorNested13[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] = AndXorNested13[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]: AndXor14[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13] = AndXor14[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]]]: AndXorNested14[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13] = AndXorNested14[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]: AndXor15[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14] = AndXor15[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]]]: AndXorNested15[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14] = AndXorNested15[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]: AndXor16[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15] = AndXor16[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]]]: AndXorNested16[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15] = AndXorNested16[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]: AndXor17[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16] = AndXor17[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]]]: AndXorNested17[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16] = AndXorNested17[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17]: AndXor18[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17] = AndXor18[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]], B17[_[_]]]: AndXorNested18[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17] = AndXorNested18[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18]: AndXor19[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18] = AndXor19[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]], B17[_[_]], B18[_[_]]]: AndXorNested19[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18] = AndXorNested19[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19]: AndXor20[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19] = AndXor20[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]], B17[_[_]], B18[_[_]], B19[_[_]]]: AndXorNested20[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19] = AndXorNested20[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20]: AndXor21[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20] = AndXor21[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]], B17[_[_]], B18[_[_]], B19[_[_]], B20[_[_]]]: AndXorNested21[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20] = AndXorNested21[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21]: AndXor22[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21] = AndXor22[A1, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]], B17[_[_]], B18[_[_]], B19[_[_]], B20[_[_]], B21[_[_]]]: AndXorNested22[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21] = AndXorNested22[Lambda[f[_] => f[A1]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21]

  type Prod[F[_]] = Prod1[F, A1]
  object Prod {
    def apply[F[_]](p: F[A1]): Prod[F] = Prod1[F, A1](p)
  }

  type Cop[F[_]] = Cop1[F, A1]
  object Cop {
    def apply[F[_]](c: F[A1]): Cop[F] = Cop1[F, A1](c)
  }

  def deriving[TC[_], F[_]](implicit t0: TC[F[A1]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        d.contramap(t0)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        a.map(t0)(x => f(Cop1[F, A1](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        a.contramap(t0)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =
        a.map(t0)(x => f(Prod1[F, A1](x)))
    }

  def derivingId[TC[_]](implicit t0: TC[A1]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]
}

object AndXor1 {
  def apply[A1]: AndXor1[A1] =
    new AndXor1[A1] {}
}
