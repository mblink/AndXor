package andxor

import andxor.syntax.ffunctor._
import andxor.syntax.ftraverse._
import andxor.types._
import cats.syntax.either._
import cats.{Applicative, Apply, Functor, Id, Monoid, MonoidK, ~>}
import cats.instances.vector._

trait AndXorNested5[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]]] extends AndXor {

  def apply[B1]: AndXorNested6[A1, A2, A3, A4, A5, FConst[B1]#T] = AndXorNested6[A1, A2, A3, A4, A5, FConst[B1]#T]
  def nest[B1[_[_]]]: AndXorNested6[A1, A2, A3, A4, A5, B1] = AndXorNested6[A1, A2, A3, A4, A5, B1]

  def apply[B1, B2]: AndXorNested7[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T] = AndXorNested7[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested7[A1, A2, A3, A4, A5, B1, B2] = AndXorNested7[A1, A2, A3, A4, A5, B1, B2]

  def apply[B1, B2, B3]: AndXorNested8[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T] = AndXorNested8[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]]]: AndXorNested8[A1, A2, A3, A4, A5, B1, B2, B3] = AndXorNested8[A1, A2, A3, A4, A5, B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXorNested9[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T] = AndXorNested9[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]]]: AndXorNested9[A1, A2, A3, A4, A5, B1, B2, B3, B4] = AndXorNested9[A1, A2, A3, A4, A5, B1, B2, B3, B4]

  def apply[B1, B2, B3, B4, B5]: AndXorNested10[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T] = AndXorNested10[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]]]: AndXorNested10[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5] = AndXorNested10[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5]

  def apply[B1, B2, B3, B4, B5, B6]: AndXorNested11[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T] = AndXorNested11[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]]]: AndXorNested11[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6] = AndXorNested11[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6]

  def apply[B1, B2, B3, B4, B5, B6, B7]: AndXorNested12[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T] = AndXorNested12[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]]]: AndXorNested12[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7] = AndXorNested12[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8]: AndXorNested13[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T] = AndXorNested13[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]]]: AndXorNested13[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8] = AndXorNested13[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9]: AndXorNested14[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T] = AndXorNested14[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]]]: AndXorNested14[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXorNested14[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]: AndXorNested15[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T] = AndXorNested15[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]]]: AndXorNested15[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXorNested15[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]: AndXorNested16[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T] = AndXorNested16[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]]]: AndXorNested16[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] = AndXorNested16[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]: AndXorNested17[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T] = AndXorNested17[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]]]: AndXorNested17[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] = AndXorNested17[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]: AndXorNested18[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T] = AndXorNested18[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]]]: AndXorNested18[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13] = AndXorNested18[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]: AndXorNested19[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T] = AndXorNested19[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]]]: AndXorNested19[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14] = AndXorNested19[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]: AndXorNested20[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T, FConst[B15]#T] = AndXorNested20[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T, FConst[B15]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]]]: AndXorNested20[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15] = AndXorNested20[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]: AndXorNested21[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T, FConst[B15]#T, FConst[B16]#T] = AndXorNested21[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T, FConst[B15]#T, FConst[B16]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]]]: AndXorNested21[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16] = AndXorNested21[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17]: AndXorNested22[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T, FConst[B15]#T, FConst[B16]#T, FConst[B17]#T] = AndXorNested22[A1, A2, A3, A4, A5, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T, FConst[B15]#T, FConst[B16]#T, FConst[B17]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]], B17[_[_]]]: AndXorNested22[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17] = AndXorNested22[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17]

  type Prod[F[_]] = Prod5[Id, A1[F], A2[F], A3[F], A4[F], A5[F]]
  object Prod {
    def apply[F[_]](p: (A1[F], A2[F], A3[F], A4[F], A5[F])): Prod[F] = Prod5[Id, A1[F], A2[F], A3[F], A4[F], A5[F]](p)
  }

  type Cop[F[_]] = Cop5[Id, A1[F], A2[F], A3[F], A4[F], A5[F]]
  object Cop {
    def apply[F[_]](c: Either[A1[F], Either[A2[F], Either[A3[F], Either[A4[F], A5[F]]]]]): Cop[F] = Cop5[Id, A1[F], A2[F], A3[F], A4[F], A5[F]](c)
  }

  object instances {
    implicit def axoProd5Instance(implicit ft0: FTraverse[A1, Applicative], ft1: FTraverse[A2, Applicative], ft2: FTraverse[A3, Applicative], ft3: FTraverse[A4, Applicative], ft4: FTraverse[A5, Applicative]): FFunctor[Prod] with FTraverseProd[Prod] =
      new FFunctor[Prod] with FTraverseProd[Prod] {
        def map[F[_], G[_]](p: Prod5[Id, A1[F], A2[F], A3[F], A4[F], A5[F]])(nt: F ~> G): Prod5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]] =
          Prod5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]]((ft0.map(p.t1)(nt), ft1.map(p.t2)(nt), ft2.map(p.t3)(nt), ft3.map(p.t4)(nt), ft4.map(p.t5)(nt)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod5[Id, A1[F], A2[F], A3[F], A4[F], A5[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Prod5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]]] =
          Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].map(ft0.traverse(p.t1)(f))((i0: A1[G]) => (i1: A2[G]) => (i2: A3[G]) => (i3: A4[G]) => (i4: A5[G]) => Prod5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]]((i0, i1, i2, i3, i4))))(ft1.traverse(p.t2)(f)))(ft2.traverse(p.t3)(f)))(ft3.traverse(p.t4)(f)))(ft4.traverse(p.t5)(f))
      }

    implicit def axoProd5FoldMap(implicit fm0: FoldMap[A1, A1], fm1: FoldMap[A2, A2], fm2: FoldMap[A3, A3], fm3: FoldMap[A4, A4], fm4: FoldMap[A5, A5]): FoldMap[Prod, Cop] =
      new FoldMap[Prod, Cop] {
        def emptyProd[F[_]](implicit PE: MonoidK[F]): Prod[F] =
          Prod((fm0.emptyProd, fm1.emptyProd, fm2.emptyProd, fm3.emptyProd, fm4.emptyProd))

        def unconsAll[F[_], G[_]](p: Prod5[Id, A1[F], A2[F], A3[F], A4[F], A5[F]])(implicit U: Uncons[F, G]): (List[Cop5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]]], Prod5[Id, A1[F], A2[F], A3[F], A4[F], A5[F]]) = {
          val (h1, t1) = fm0.unconsAll(p.t1)
          val (h2, t2) = fm1.unconsAll(p.t2)
          val (h3, t3) = fm2.unconsAll(p.t3)
          val (h4, t4) = fm3.unconsAll(p.t4)
          val (h5, t5) = fm4.unconsAll(p.t5)
          (
            List(h1.map(Inj[Cop5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]], A1[G]].apply(_)), h2.map(Inj[Cop5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]], A2[G]].apply(_)), h3.map(Inj[Cop5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]], A3[G]].apply(_)), h4.map(Inj[Cop5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]], A4[G]].apply(_)), h5.map(Inj[Cop5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]], A5[G]].apply(_))).flatten,
            Prod5[Id, A1[F], A2[F], A3[F], A4[F], A5[F]]((t1, t2, t3, t4, t5)))
        }

        def unconsOne[F[_], G[_]](p: Prod5[Id, A1[F], A2[F], A3[F], A4[F], A5[F]], c: Cop5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]])(implicit U: Uncons[F, G]): (Option[Cop5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]]], Prod5[Id, A1[F], A2[F], A3[F], A4[F], A5[F]]) =
          c.run match {

            case Left(x) =>
              val (h, t) = fm0.unconsOne(p.t1, x)
              (h.map(v => Cop5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]](Left(v))), Prod5[Id, A1[F], A2[F], A3[F], A4[F], A5[F]]((t, p.t2, p.t3, p.t4, p.t5)))

            case Right(Left(x)) =>
              val (h, t) = fm1.unconsOne(p.t2, x)
              (h.map(v => Cop5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]](Right(Left(v)))), Prod5[Id, A1[F], A2[F], A3[F], A4[F], A5[F]]((p.t1, t, p.t3, p.t4, p.t5)))

            case Right(Right(Left(x))) =>
              val (h, t) = fm2.unconsOne(p.t3, x)
              (h.map(v => Cop5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]](Right(Right(Left(v))))), Prod5[Id, A1[F], A2[F], A3[F], A4[F], A5[F]]((p.t1, p.t2, t, p.t4, p.t5)))

            case Right(Right(Right(Left(x)))) =>
              val (h, t) = fm3.unconsOne(p.t4, x)
              (h.map(v => Cop5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]](Right(Right(Right(Left(v)))))), Prod5[Id, A1[F], A2[F], A3[F], A4[F], A5[F]]((p.t1, p.t2, p.t3, t, p.t5)))

            case Right(Right(Right(Right(x)))) =>
              val (h, t) = fm4.unconsOne(p.t5, x)
              (h.map(v => Cop5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]](Right(Right(Right(Right(v)))))), Prod5[Id, A1[F], A2[F], A3[F], A4[F], A5[F]]((p.t1, p.t2, p.t3, p.t4, t)))

          }
      }

    implicit def axoCop5Instance(implicit ft0: FTraverse[A1, Functor], ft1: FTraverse[A2, Functor], ft2: FTraverse[A3, Functor], ft3: FTraverse[A4, Functor], ft4: FTraverse[A5, Functor]): FFunctor[Cop] with FTraverseCop[Cop] =
      new FFunctor[Cop] with FTraverseCop[Cop] {
        def map[F[_], G[_]](c: Cop5[Id, A1[F], A2[F], A3[F], A4[F], A5[F]])(nt: F ~> G): Cop5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]] =
          Cop5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]](c.run.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.map(nt))))))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop5[Id, A1[F], A2[F], A3[F], A4[F], A5[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Cop5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]]] =
          c.run match {

            case Left(x) => Functor[A].map(x.traverse(f))(y => Cop5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]](Left(y)))

            case Right(Left(x)) => Functor[A].map(x.traverse(f))(y => Cop5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]](Right(Left(y))))

            case Right(Right(Left(x))) => Functor[A].map(x.traverse(f))(y => Cop5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]](Right(Right(Left(y)))))

            case Right(Right(Right(Left(x)))) => Functor[A].map(x.traverse(f))(y => Cop5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]](Right(Right(Right(Left(y))))))

            case Right(Right(Right(Right(x)))) => Functor[A].map(x.traverse(f))(y => Cop5[Id, A1[G], A2[G], A3[G], A4[G], A5[G]](Right(Right(Right(Right(y))))))

          }
      }
  }

  def deriving[TC[_], F[_]](implicit t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose5(t0, t1, t2, t3, t4)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly5(t0, t1, t2, t3, t4)(x => f(Cop5[Id, A1[F], A2[F], A3[F], A4[F], A5[F]](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide5(t0, t1, t2, t3, t4)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply5(t0, t1, t2, t3, t4) {
          case (i0, i1, i2, i3, i4) =>
            f(Prod5[Id, A1[F], A2[F], A3[F], A4[F], A5[F]]((i0, i1, i2, i3, i4)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1[Id]], t1: TC[A2[Id]], t2: TC[A3[Id]], t3: TC[A4[Id]], t4: TC[A5[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = deriving[Inj[Prod[F], ?], F].divide
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]] = deriving[Inj[Prod[F], ?], F].choose
    implicit def injProdToVecCopEv[F[_]]: Inj[Vector[Cop[F]], Prod[F]] = deriving[Inj[Vector[Cop[F]], ?], F].divide
  }

}

object AndXorNested5 {
  def apply[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]]]: AndXorNested5[A1, A2, A3, A4, A5] =
    new AndXorNested5[A1, A2, A3, A4, A5] {}
}

trait AndXor5[A1, A2, A3, A4, A5] extends AndXor {

  def apply[B1]: AndXor6[A1, A2, A3, A4, A5, B1] = AndXor6[A1, A2, A3, A4, A5, B1]
  def nest[B1[_[_]]]: AndXorNested6[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1] = AndXorNested6[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1]

  def apply[B1, B2]: AndXor7[A1, A2, A3, A4, A5, B1, B2] = AndXor7[A1, A2, A3, A4, A5, B1, B2]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested7[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2] = AndXorNested7[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2]

  def apply[B1, B2, B3]: AndXor8[A1, A2, A3, A4, A5, B1, B2, B3] = AndXor8[A1, A2, A3, A4, A5, B1, B2, B3]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]]]: AndXorNested8[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3] = AndXorNested8[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXor9[A1, A2, A3, A4, A5, B1, B2, B3, B4] = AndXor9[A1, A2, A3, A4, A5, B1, B2, B3, B4]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]]]: AndXorNested9[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4] = AndXorNested9[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4]

  def apply[B1, B2, B3, B4, B5]: AndXor10[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5] = AndXor10[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]]]: AndXorNested10[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5] = AndXorNested10[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5]

  def apply[B1, B2, B3, B4, B5, B6]: AndXor11[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6] = AndXor11[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]]]: AndXorNested11[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6] = AndXorNested11[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6]

  def apply[B1, B2, B3, B4, B5, B6, B7]: AndXor12[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7] = AndXor12[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]]]: AndXorNested12[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7] = AndXorNested12[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8]: AndXor13[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8] = AndXor13[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]]]: AndXorNested13[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7, B8] = AndXorNested13[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7, B8]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9]: AndXor14[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXor14[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]]]: AndXorNested14[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXorNested14[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]: AndXor15[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXor15[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]]]: AndXorNested15[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXorNested15[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]: AndXor16[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] = AndXor16[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]]]: AndXorNested16[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] = AndXorNested16[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]: AndXor17[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] = AndXor17[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]]]: AndXorNested17[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] = AndXorNested17[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]: AndXor18[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13] = AndXor18[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]]]: AndXorNested18[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13] = AndXorNested18[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]: AndXor19[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14] = AndXor19[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]]]: AndXorNested19[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14] = AndXorNested19[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]: AndXor20[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15] = AndXor20[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]]]: AndXorNested20[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15] = AndXorNested20[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]: AndXor21[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16] = AndXor21[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]]]: AndXorNested21[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16] = AndXorNested21[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17]: AndXor22[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17] = AndXor22[A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]], B15[_[_]], B16[_[_]], B17[_[_]]]: AndXorNested22[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17] = AndXorNested22[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17]

  type Prod[F[_]] = Prod5[F, A1, A2, A3, A4, A5]
  object Prod {
    def apply[F[_]](p: (F[A1], F[A2], F[A3], F[A4], F[A5])): Prod[F] = Prod5[F, A1, A2, A3, A4, A5](p)
  }

  type Cop[F[_]] = Cop5[F, A1, A2, A3, A4, A5]
  object Cop {
    def apply[F[_]](c: Either[F[A1], Either[F[A2], Either[F[A3], Either[F[A4], F[A5]]]]]): Cop[F] = Cop5[F, A1, A2, A3, A4, A5](c)
  }

  def deriving[TC[_], F[_]](implicit t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose5(t0, t1, t2, t3, t4)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly5(t0, t1, t2, t3, t4)(x => f(Cop5[F, A1, A2, A3, A4, A5](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide5(t0, t1, t2, t3, t4)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply5(t0, t1, t2, t3, t4) {
          case (i0, i1, i2, i3, i4) =>
            f(Prod5[F, A1, A2, A3, A4, A5]((i0, i1, i2, i3, i4)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1], t1: TC[A2], t2: TC[A3], t3: TC[A4], t4: TC[A5]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = deriving[Inj[Prod[F], ?], F].divide
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]] = deriving[Inj[Prod[F], ?], F].choose
    implicit def injProdToVecCopEv[F[_]]: Inj[Vector[Cop[F]], Prod[F]] = deriving[Inj[Vector[Cop[F]], ?], F].divide
  }

}

object AndXor5 {
  def apply[A1, A2, A3, A4, A5]: AndXor5[A1, A2, A3, A4, A5] =
    new AndXor5[A1, A2, A3, A4, A5] {}
}
