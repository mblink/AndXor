package andxor

import andxor.types._
import andxor.syntax.ffunctor._
import andxor.syntax.ftraverse._
import cats.syntax.either._
import cats.{Applicative, Apply, Functor, Id, MonoidK, ~>}

trait AndXorNested6[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]]] extends AndXor {

  def *:[B](@annotation.unused a: AndXor1[B]): AndXorNested7[Lambda[f[_] => f[B]], A1, A2, A3, A4, A5, A6] = AndXorNested7[Lambda[f[_] => f[B]], A1, A2, A3, A4, A5, A6]
  def *:[B[_[_]]](@annotation.unused a: AndXorNested1[B]): AndXorNested7[B, A1, A2, A3, A4, A5, A6] = AndXorNested7[B, A1, A2, A3, A4, A5, A6]

  def apply[B1]: AndXorNested7[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]]] = AndXorNested7[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]]]
  def nest[B1[_[_]]]: AndXorNested7[A1, A2, A3, A4, A5, A6, B1] = AndXorNested7[A1, A2, A3, A4, A5, A6, B1]

  def apply[B1, B2]: AndXorNested8[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]]] = AndXorNested8[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]]]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested8[A1, A2, A3, A4, A5, A6, B1, B2] = AndXorNested8[A1, A2, A3, A4, A5, A6, B1, B2]

  def apply[B1, B2, B3]: AndXorNested9[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]]] = AndXorNested9[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]]]: AndXorNested9[A1, A2, A3, A4, A5, A6, B1, B2, B3] = AndXorNested9[A1, A2, A3, A4, A5, A6, B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXorNested10[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]]] = AndXorNested10[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]]]: AndXorNested10[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4] = AndXorNested10[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4]

  def apply[B1, B2, B3, B4, B5]: AndXorNested11[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]]] = AndXorNested11[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]]]: AndXorNested11[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5] = AndXorNested11[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5]

  def apply[B1, B2, B3, B4, B5, B6]: AndXorNested12[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]]] = AndXorNested12[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]]]: AndXorNested12[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6] = AndXorNested12[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6]

  def apply[B1, B2, B3, B4, B5, B6, B7]: AndXorNested13[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]]] = AndXorNested13[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]]]: AndXorNested13[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7] = AndXorNested13[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8]: AndXorNested14[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]]] = AndXorNested14[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]]]: AndXorNested14[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8] = AndXorNested14[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9]: AndXorNested15[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]]] = AndXorNested15[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]]]: AndXorNested15[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXorNested15[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]: AndXorNested16[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]]] = AndXorNested16[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]]]: AndXorNested16[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXorNested16[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]: AndXorNested17[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]]] = AndXorNested17[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]]]: AndXorNested17[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] = AndXorNested17[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]: AndXorNested18[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]]] = AndXorNested18[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]]]: AndXorNested18[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] = AndXorNested18[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]: AndXorNested19[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]]] = AndXorNested19[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]]]: AndXorNested19[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13] = AndXorNested19[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]: AndXorNested20[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]]] = AndXorNested20[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]]]: AndXorNested20[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14] = AndXorNested20[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]: AndXorNested21[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]], Lambda[f[_] => f[B15]]] = AndXorNested21[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]], Lambda[f[_] => f[B15]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]]]: AndXorNested21[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15] = AndXorNested21[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]: AndXorNested22[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]], Lambda[f[_] => f[B15]], Lambda[f[_] => f[B16]]] = AndXorNested22[A1, A2, A3, A4, A5, A6, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]], Lambda[f[_] => f[B5]], Lambda[f[_] => f[B6]], Lambda[f[_] => f[B7]], Lambda[f[_] => f[B8]], Lambda[f[_] => f[B9]], Lambda[f[_] => f[B10]], Lambda[f[_] => f[B11]], Lambda[f[_] => f[B12]], Lambda[f[_] => f[B13]], Lambda[f[_] => f[B14]], Lambda[f[_] => f[B15]], Lambda[f[_] => f[B16]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]]]: AndXorNested22[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16] = AndXorNested22[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]

  type Prod[F[_]] = Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]
  object Prod {
    def apply[F[_]](p: (A1[F], A2[F], A3[F], A4[F], A5[F], A6[F])): Prod[F] = Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]](p)
  }

  type Cop[F[_]] = Cop6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]
  object Cop {
    def apply[F[_]](c: Either[A1[F], Either[A2[F], Either[A3[F], Either[A4[F], Either[A5[F], A6[F]]]]]]): Cop[F] = Cop6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]](c)
  }

  object instances {
    implicit def axoProd6Instance(implicit ft0: FTraverse[A1, Applicative], ft1: FTraverse[A2, Applicative], ft2: FTraverse[A3, Applicative], ft3: FTraverse[A4, Applicative], ft4: FTraverse[A5, Applicative], ft5: FTraverse[A6, Applicative]): FFunctor[Prod] with FTraverseProd[Prod] =
      new FFunctor[Prod] with FTraverseProd[Prod] {
        def map[F[_], G[_]](p: Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]])(nt: F ~> G): Prod6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]] =
          Prod6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]]((ft0.map(p.t1)(nt), ft1.map(p.t2)(nt), ft2.map(p.t3)(nt), ft3.map(p.t4)(nt), ft4.map(p.t5)(nt), ft5.map(p.t6)(nt)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Prod6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]]] =
          Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].map(ft0.traverse(p.t1)(f))((i0: A1[G]) => (i1: A2[G]) => (i2: A3[G]) => (i3: A4[G]) => (i4: A5[G]) => (i5: A6[G]) => Prod6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]]((i0, i1, i2, i3, i4, i5))))(ft1.traverse(p.t2)(f)))(ft2.traverse(p.t3)(f)))(ft3.traverse(p.t4)(f)))(ft4.traverse(p.t5)(f)))(ft5.traverse(p.t6)(f))
      }

    implicit def axoProd6FoldMap(implicit fm0: FoldMap[A1, A1], fm1: FoldMap[A2, A2], fm2: FoldMap[A3, A3], fm3: FoldMap[A4, A4], fm4: FoldMap[A5, A5], fm5: FoldMap[A6, A6]): FoldMap[Prod, Cop] =
      new FoldMap[Prod, Cop] {
        def emptyProd[F[_]](implicit PE: MonoidK[F]): Prod[F] =
          Prod((fm0.emptyProd, fm1.emptyProd, fm2.emptyProd, fm3.emptyProd, fm4.emptyProd, fm5.emptyProd))

        def unconsAll[F[_], G[_]](p: Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]])(implicit U: Uncons[F, G]): (List[Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]]], Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]) = {
          val (h1, t1) = fm0.unconsAll(p.t1)
          val (h2, t2) = fm1.unconsAll(p.t2)
          val (h3, t3) = fm2.unconsAll(p.t3)
          val (h4, t4) = fm3.unconsAll(p.t4)
          val (h5, t5) = fm4.unconsAll(p.t5)
          val (h6, t6) = fm5.unconsAll(p.t6)
          (
            List(h1.map(Inj[Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]], A1[G]].apply(_)), h2.map(Inj[Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]], A2[G]].apply(_)), h3.map(Inj[Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]], A3[G]].apply(_)), h4.map(Inj[Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]], A4[G]].apply(_)), h5.map(Inj[Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]], A5[G]].apply(_)), h6.map(Inj[Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]], A6[G]].apply(_))).flatten,
            Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]((t1, t2, t3, t4, t5, t6)))
        }

        def unconsOne[F[_], G[_]](p: Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]], c: Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]])(implicit U: Uncons[F, G]): (Option[Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]]], Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]) =
          c.run match {

            case Left(x) =>
              val (h, t) = fm0.unconsOne(p.t1, x)
              (h.map(v => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Left(v))), Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]((t, p.t2, p.t3, p.t4, p.t5, p.t6)))

            case Right(Left(x)) =>
              val (h, t) = fm1.unconsOne(p.t2, x)
              (h.map(v => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Right(Left(v)))), Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]((p.t1, t, p.t3, p.t4, p.t5, p.t6)))

            case Right(Right(Left(x))) =>
              val (h, t) = fm2.unconsOne(p.t3, x)
              (h.map(v => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Right(Right(Left(v))))), Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]((p.t1, p.t2, t, p.t4, p.t5, p.t6)))

            case Right(Right(Right(Left(x)))) =>
              val (h, t) = fm3.unconsOne(p.t4, x)
              (h.map(v => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Right(Right(Right(Left(v)))))), Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]((p.t1, p.t2, p.t3, t, p.t5, p.t6)))

            case Right(Right(Right(Right(Left(x))))) =>
              val (h, t) = fm4.unconsOne(p.t5, x)
              (h.map(v => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Right(Right(Right(Right(Left(v))))))), Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]((p.t1, p.t2, p.t3, p.t4, t, p.t6)))

            case Right(Right(Right(Right(Right(x))))) =>
              val (h, t) = fm5.unconsOne(p.t6, x)
              (h.map(v => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Right(Right(Right(Right(Right(v))))))), Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, t)))

          }
      }

    implicit def axoCop6Instance(implicit ft0: FTraverse[A1, Functor], ft1: FTraverse[A2, Functor], ft2: FTraverse[A3, Functor], ft3: FTraverse[A4, Functor], ft4: FTraverse[A5, Functor], ft5: FTraverse[A6, Functor]): FFunctor[Cop] with FTraverseCop[Cop] =
      new FFunctor[Cop] with FTraverseCop[Cop] {
        def map[F[_], G[_]](c: Cop6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]])(nt: F ~> G): Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]] =
          Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](c.run.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.map(nt)))))))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]]] =
          c.run match {

            case Left(x) => Functor[A].map(x.traverse(f))(y => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Left(y)))

            case Right(Left(x)) => Functor[A].map(x.traverse(f))(y => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Right(Left(y))))

            case Right(Right(Left(x))) => Functor[A].map(x.traverse(f))(y => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Right(Right(Left(y)))))

            case Right(Right(Right(Left(x)))) => Functor[A].map(x.traverse(f))(y => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Right(Right(Right(Left(y))))))

            case Right(Right(Right(Right(Left(x))))) => Functor[A].map(x.traverse(f))(y => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Right(Right(Right(Right(Left(y)))))))

            case Right(Right(Right(Right(Right(x))))) => Functor[A].map(x.traverse(f))(y => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Right(Right(Right(Right(Right(y)))))))

          }
      }
  }

  def deriving[TC[_], F[_]](implicit t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose6(t0, t1, t2, t3, t4, t5)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly6(t0, t1, t2, t3, t4, t5)(x => f(Cop6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide6(t0, t1, t2, t3, t4, t5)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply6(t0, t1, t2, t3, t4, t5) {
          case (i0, i1, i2, i3, i4, i5) =>
            f(Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]((i0, i1, i2, i3, i4, i5)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1[Id]], t1: TC[A2[Id]], t2: TC[A3[Id]], t3: TC[A4[Id]], t4: TC[A5[Id]], t5: TC[A6[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]
}

object AndXorNested6 {
  def apply[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]]]: AndXorNested6[A1, A2, A3, A4, A5, A6] =
    new AndXorNested6[A1, A2, A3, A4, A5, A6] {}
}

trait AndXor6[A1, A2, A3, A4, A5, A6] extends AndXor {

  def *:[B](@annotation.unused a: AndXor1[B]): AndXor7[B, A1, A2, A3, A4, A5, A6] = AndXor7[B, A1, A2, A3, A4, A5, A6]
  def *:[B[_[_]]](@annotation.unused a: AndXorNested1[B]): AndXorNested7[B, Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]]] = AndXorNested7[B, Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]]]

  def apply[B1]: AndXor7[A1, A2, A3, A4, A5, A6, B1] = AndXor7[A1, A2, A3, A4, A5, A6, B1]
  def nest[B1[_[_]]]: AndXorNested7[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1] = AndXorNested7[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1]

  def apply[B1, B2]: AndXor8[A1, A2, A3, A4, A5, A6, B1, B2] = AndXor8[A1, A2, A3, A4, A5, A6, B1, B2]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested8[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2] = AndXorNested8[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2]

  def apply[B1, B2, B3]: AndXor9[A1, A2, A3, A4, A5, A6, B1, B2, B3] = AndXor9[A1, A2, A3, A4, A5, A6, B1, B2, B3]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]]]: AndXorNested9[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3] = AndXorNested9[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXor10[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4] = AndXor10[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]]]: AndXorNested10[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4] = AndXorNested10[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4]

  def apply[B1, B2, B3, B4, B5]: AndXor11[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5] = AndXor11[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]]]: AndXorNested11[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5] = AndXorNested11[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5]

  def apply[B1, B2, B3, B4, B5, B6]: AndXor12[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6] = AndXor12[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]]]: AndXorNested12[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6] = AndXorNested12[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6]

  def apply[B1, B2, B3, B4, B5, B6, B7]: AndXor13[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7] = AndXor13[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]]]: AndXorNested13[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6, B7] = AndXorNested13[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6, B7]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8]: AndXor14[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8] = AndXor14[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]]]: AndXorNested14[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6, B7, B8] = AndXorNested14[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6, B7, B8]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9]: AndXor15[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXor15[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]]]: AndXorNested15[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXorNested15[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6, B7, B8, B9]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]: AndXor16[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXor16[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]]]: AndXorNested16[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXorNested16[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]: AndXor17[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] = AndXor17[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]]]: AndXorNested17[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] = AndXorNested17[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]: AndXor18[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] = AndXor18[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]]]: AndXorNested18[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] = AndXorNested18[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]: AndXor19[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13] = AndXor19[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]]]: AndXorNested19[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13] = AndXorNested19[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]: AndXor20[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14] = AndXor20[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]]]: AndXorNested20[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14] = AndXorNested20[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]: AndXor21[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15] = AndXor21[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]]]: AndXorNested21[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15] = AndXorNested21[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]: AndXor22[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16] = AndXor22[A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]]]: AndXorNested22[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16] = AndXorNested22[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]

  type Prod[F[_]] = Prod6[F, A1, A2, A3, A4, A5, A6]
  object Prod {
    def apply[F[_]](p: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6])): Prod[F] = Prod6[F, A1, A2, A3, A4, A5, A6](p)
  }

  type Cop[F[_]] = Cop6[F, A1, A2, A3, A4, A5, A6]
  object Cop {
    def apply[F[_]](c: Either[F[A1], Either[F[A2], Either[F[A3], Either[F[A4], Either[F[A5], F[A6]]]]]]): Cop[F] = Cop6[F, A1, A2, A3, A4, A5, A6](c)
  }

  def deriving[TC[_], F[_]](implicit t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose6(t0, t1, t2, t3, t4, t5)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly6(t0, t1, t2, t3, t4, t5)(x => f(Cop6[F, A1, A2, A3, A4, A5, A6](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide6(t0, t1, t2, t3, t4, t5)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply6(t0, t1, t2, t3, t4, t5) {
          case (i0, i1, i2, i3, i4, i5) =>
            f(Prod6[F, A1, A2, A3, A4, A5, A6]((i0, i1, i2, i3, i4, i5)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1], t1: TC[A2], t2: TC[A3], t3: TC[A4], t4: TC[A5], t5: TC[A6]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]
}

object AndXor6 {
  def apply[A1, A2, A3, A4, A5, A6]: AndXor6[A1, A2, A3, A4, A5, A6] =
    new AndXor6[A1, A2, A3, A4, A5, A6] {}
}
