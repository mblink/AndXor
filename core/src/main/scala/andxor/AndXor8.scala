package andxor

import andxor.syntax.ffunctor._
import andxor.syntax.ftraverse._
import andxor.types._
import cats.syntax.either._
import cats.{Applicative, Apply, Functor, Id, Monoid, MonoidK, ~>}
import cats.instances.vector._

trait AndXorNested8[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]]] extends AndXor {

  def apply[B1]: AndXorNested9[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T] = AndXorNested9[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T]
  def nest[B1[_[_]]]: AndXorNested9[A1, A2, A3, A4, A5, A6, A7, A8, B1] = AndXorNested9[A1, A2, A3, A4, A5, A6, A7, A8, B1]

  def apply[B1, B2]: AndXorNested10[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T] = AndXorNested10[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested10[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2] = AndXorNested10[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2]

  def apply[B1, B2, B3]: AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T] = AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]]]: AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3] = AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T] = AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]]]: AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4] = AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4]

  def apply[B1, B2, B3, B4, B5]: AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T] = AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]]]: AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5] = AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5]

  def apply[B1, B2, B3, B4, B5, B6]: AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T] = AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]]]: AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6] = AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6]

  def apply[B1, B2, B3, B4, B5, B6, B7]: AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T] = AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]]]: AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7] = AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8]: AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T] = AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]]]: AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8] = AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9]: AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T] = AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]]]: AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]: AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T] = AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]]]: AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]: AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T] = AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]]]: AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] = AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]: AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T] = AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]]]: AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] = AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]: AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T] = AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]]]: AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13] = AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]: AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T] = AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T, FConst[B13]#T, FConst[B14]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]]]: AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14] = AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]

  type Prod[F[_]] = Prod8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]]
  object Prod {
    def apply[F[_]](p: (A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F])): Prod[F] = Prod8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]](p)
  }

  type Cop[F[_]] = Cop8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]]
  object Cop {
    def apply[F[_]](c: Either[A1[F], Either[A2[F], Either[A3[F], Either[A4[F], Either[A5[F], Either[A6[F], Either[A7[F], A8[F]]]]]]]]): Cop[F] = Cop8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]](c)
  }

  object instances {
    implicit def axoProd8Instance(implicit ft0: FTraverse[A1, Applicative], ft1: FTraverse[A2, Applicative], ft2: FTraverse[A3, Applicative], ft3: FTraverse[A4, Applicative], ft4: FTraverse[A5, Applicative], ft5: FTraverse[A6, Applicative], ft6: FTraverse[A7, Applicative], ft7: FTraverse[A8, Applicative]): FFunctor[Prod] with FTraverseProd[Prod] =
      new FFunctor[Prod] with FTraverseProd[Prod] {
        def map[F[_], G[_]](p: Prod8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]])(nt: F ~> G): Prod8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]] =
          Prod8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]]((ft0.map(p.t1)(nt), ft1.map(p.t2)(nt), ft2.map(p.t3)(nt), ft3.map(p.t4)(nt), ft4.map(p.t5)(nt), ft5.map(p.t6)(nt), ft6.map(p.t7)(nt), ft7.map(p.t8)(nt)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Prod8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]]] =
          Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].map(ft0.traverse(p.t1)(f))((i0: A1[G]) => (i1: A2[G]) => (i2: A3[G]) => (i3: A4[G]) => (i4: A5[G]) => (i5: A6[G]) => (i6: A7[G]) => (i7: A8[G]) => Prod8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]]((i0, i1, i2, i3, i4, i5, i6, i7))))(ft1.traverse(p.t2)(f)))(ft2.traverse(p.t3)(f)))(ft3.traverse(p.t4)(f)))(ft4.traverse(p.t5)(f)))(ft5.traverse(p.t6)(f)))(ft6.traverse(p.t7)(f)))(ft7.traverse(p.t8)(f))
      }

    implicit def axoProd8FoldMap(implicit fm0: FoldMap[A1, A1], fm1: FoldMap[A2, A2], fm2: FoldMap[A3, A3], fm3: FoldMap[A4, A4], fm4: FoldMap[A5, A5], fm5: FoldMap[A6, A6], fm6: FoldMap[A7, A7], fm7: FoldMap[A8, A8]): FoldMap[Prod, Cop] =
      new FoldMap[Prod, Cop] {
        def emptyProd[F[_]](implicit PE: MonoidK[F]): Prod[F] =
          Prod((fm0.emptyProd, fm1.emptyProd, fm2.emptyProd, fm3.emptyProd, fm4.emptyProd, fm5.emptyProd, fm6.emptyProd, fm7.emptyProd))

        def unconsAll[F[_], G[_]](p: Prod8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]])(implicit U: Uncons[F, G]): (List[Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]]], Prod8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]]) = {
          val (h1, t1) = fm0.unconsAll(p.t1)
          val (h2, t2) = fm1.unconsAll(p.t2)
          val (h3, t3) = fm2.unconsAll(p.t3)
          val (h4, t4) = fm3.unconsAll(p.t4)
          val (h5, t5) = fm4.unconsAll(p.t5)
          val (h6, t6) = fm5.unconsAll(p.t6)
          val (h7, t7) = fm6.unconsAll(p.t7)
          val (h8, t8) = fm7.unconsAll(p.t8)
          (
            List(h1.map(Inj[Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]], A1[G]].apply(_)), h2.map(Inj[Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]], A2[G]].apply(_)), h3.map(Inj[Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]], A3[G]].apply(_)), h4.map(Inj[Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]], A4[G]].apply(_)), h5.map(Inj[Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]], A5[G]].apply(_)), h6.map(Inj[Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]], A6[G]].apply(_)), h7.map(Inj[Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]], A7[G]].apply(_)), h8.map(Inj[Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]], A8[G]].apply(_))).flatten,
            Prod8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]]((t1, t2, t3, t4, t5, t6, t7, t8)))
        }

        def unconsOne[F[_], G[_]](p: Prod8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]], c: Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]])(implicit U: Uncons[F, G]): (Option[Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]]], Prod8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]]) =
          c.run match {

            case Left(x) =>
              val (h, t) = fm0.unconsOne(p.t1, x)
              (h.map(v => Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]](Left(v))), Prod8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]]((t, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8)))

            case Right(Left(x)) =>
              val (h, t) = fm1.unconsOne(p.t2, x)
              (h.map(v => Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]](Right(Left(v)))), Prod8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]]((p.t1, t, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8)))

            case Right(Right(Left(x))) =>
              val (h, t) = fm2.unconsOne(p.t3, x)
              (h.map(v => Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]](Right(Right(Left(v))))), Prod8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]]((p.t1, p.t2, t, p.t4, p.t5, p.t6, p.t7, p.t8)))

            case Right(Right(Right(Left(x)))) =>
              val (h, t) = fm3.unconsOne(p.t4, x)
              (h.map(v => Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]](Right(Right(Right(Left(v)))))), Prod8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]]((p.t1, p.t2, p.t3, t, p.t5, p.t6, p.t7, p.t8)))

            case Right(Right(Right(Right(Left(x))))) =>
              val (h, t) = fm4.unconsOne(p.t5, x)
              (h.map(v => Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]](Right(Right(Right(Right(Left(v))))))), Prod8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]]((p.t1, p.t2, p.t3, p.t4, t, p.t6, p.t7, p.t8)))

            case Right(Right(Right(Right(Right(Left(x)))))) =>
              val (h, t) = fm5.unconsOne(p.t6, x)
              (h.map(v => Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]](Right(Right(Right(Right(Right(Left(v)))))))), Prod8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, t, p.t7, p.t8)))

            case Right(Right(Right(Right(Right(Right(Left(x))))))) =>
              val (h, t) = fm6.unconsOne(p.t7, x)
              (h.map(v => Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]](Right(Right(Right(Right(Right(Right(Left(v))))))))), Prod8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, t, p.t8)))

            case Right(Right(Right(Right(Right(Right(Right(x))))))) =>
              val (h, t) = fm7.unconsOne(p.t8, x)
              (h.map(v => Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]](Right(Right(Right(Right(Right(Right(Right(v))))))))), Prod8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, t)))

          }
      }

    implicit def axoCop8Instance(implicit ft0: FTraverse[A1, Functor], ft1: FTraverse[A2, Functor], ft2: FTraverse[A3, Functor], ft3: FTraverse[A4, Functor], ft4: FTraverse[A5, Functor], ft5: FTraverse[A6, Functor], ft6: FTraverse[A7, Functor], ft7: FTraverse[A8, Functor]): FFunctor[Cop] with FTraverseCop[Cop] =
      new FFunctor[Cop] with FTraverseCop[Cop] {
        def map[F[_], G[_]](c: Cop8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]])(nt: F ~> G): Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]] =
          Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]](c.run.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.map(nt)))))))))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]]] =
          c.run match {

            case Left(x) => Functor[A].map(x.traverse(f))(y => Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]](Left(y)))

            case Right(Left(x)) => Functor[A].map(x.traverse(f))(y => Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]](Right(Left(y))))

            case Right(Right(Left(x))) => Functor[A].map(x.traverse(f))(y => Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]](Right(Right(Left(y)))))

            case Right(Right(Right(Left(x)))) => Functor[A].map(x.traverse(f))(y => Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]](Right(Right(Right(Left(y))))))

            case Right(Right(Right(Right(Left(x))))) => Functor[A].map(x.traverse(f))(y => Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]](Right(Right(Right(Right(Left(y)))))))

            case Right(Right(Right(Right(Right(Left(x)))))) => Functor[A].map(x.traverse(f))(y => Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]](Right(Right(Right(Right(Right(Left(y))))))))

            case Right(Right(Right(Right(Right(Right(Left(x))))))) => Functor[A].map(x.traverse(f))(y => Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]](Right(Right(Right(Right(Right(Right(Left(y)))))))))

            case Right(Right(Right(Right(Right(Right(Right(x))))))) => Functor[A].map(x.traverse(f))(y => Cop8[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G]](Right(Right(Right(Right(Right(Right(Right(y)))))))))

          }
      }
  }

  def deriving[TC[_], F[_]](implicit t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose8(t0, t1, t2, t3, t4, t5, t6, t7)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly8(t0, t1, t2, t3, t4, t5, t6, t7)(x => f(Cop8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide8(t0, t1, t2, t3, t4, t5, t6, t7)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply8(t0, t1, t2, t3, t4, t5, t6, t7) {
          case (i0, i1, i2, i3, i4, i5, i6, i7) =>
            f(Prod8[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F]]((i0, i1, i2, i3, i4, i5, i6, i7)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1[Id]], t1: TC[A2[Id]], t2: TC[A3[Id]], t3: TC[A4[Id]], t4: TC[A5[Id]], t5: TC[A6[Id]], t6: TC[A7[Id]], t7: TC[A8[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], *], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = deriving[Inj[Prod[F], *], F].divide
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]] = deriving[Inj[Prod[F], *], F].choose
    implicit def injProdToVecCopEv[F[_]]: Inj[Vector[Cop[F]], Prod[F]] = deriving[Inj[Vector[Cop[F]], *], F].divide
  }

}

object AndXorNested8 {
  def apply[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]]]: AndXorNested8[A1, A2, A3, A4, A5, A6, A7, A8] =
    new AndXorNested8[A1, A2, A3, A4, A5, A6, A7, A8] {}
}

trait AndXor8[A1, A2, A3, A4, A5, A6, A7, A8] extends AndXor {

  def apply[B1]: AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, B1] = AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, B1]
  def nest[B1[_[_]]]: AndXorNested9[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1] = AndXorNested9[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1]

  def apply[B1, B2]: AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2] = AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested10[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2] = AndXorNested10[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2]

  def apply[B1, B2, B3]: AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3] = AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]]]: AndXorNested11[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3] = AndXorNested11[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4] = AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]]]: AndXorNested12[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4] = AndXorNested12[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4]

  def apply[B1, B2, B3, B4, B5]: AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5] = AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]]]: AndXorNested13[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4, B5] = AndXorNested13[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4, B5]

  def apply[B1, B2, B3, B4, B5, B6]: AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6] = AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]]]: AndXorNested14[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4, B5, B6] = AndXorNested14[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4, B5, B6]

  def apply[B1, B2, B3, B4, B5, B6, B7]: AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7] = AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]]]: AndXorNested15[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4, B5, B6, B7] = AndXorNested15[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4, B5, B6, B7]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8]: AndXor16[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8] = AndXor16[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]]]: AndXorNested16[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4, B5, B6, B7, B8] = AndXorNested16[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4, B5, B6, B7, B8]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9]: AndXor17[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXor17[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]]]: AndXorNested17[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXorNested17[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]: AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]]]: AndXorNested18[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXorNested18[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]: AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] = AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]]]: AndXorNested19[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] = AndXorNested19[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]: AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] = AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]]]: AndXorNested20[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] = AndXorNested20[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]: AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13] = AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]]]: AndXorNested21[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13] = AndXorNested21[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]: AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14] = AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]], B13[_[_]], B14[_[_]]]: AndXorNested22[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14] = AndXorNested22[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14]

  type Prod[F[_]] = Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]
  object Prod {
    def apply[F[_]](p: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8])): Prod[F] = Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8](p)
  }

  type Cop[F[_]] = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]
  object Cop {
    def apply[F[_]](c: Either[F[A1], Either[F[A2], Either[F[A3], Either[F[A4], Either[F[A5], Either[F[A6], Either[F[A7], F[A8]]]]]]]]): Cop[F] = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](c)
  }

  def deriving[TC[_], F[_]](implicit t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose8(t0, t1, t2, t3, t4, t5, t6, t7)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly8(t0, t1, t2, t3, t4, t5, t6, t7)(x => f(Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide8(t0, t1, t2, t3, t4, t5, t6, t7)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply8(t0, t1, t2, t3, t4, t5, t6, t7) {
          case (i0, i1, i2, i3, i4, i5, i6, i7) =>
            f(Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((i0, i1, i2, i3, i4, i5, i6, i7)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1], t1: TC[A2], t2: TC[A3], t3: TC[A4], t4: TC[A5], t5: TC[A6], t6: TC[A7], t7: TC[A8]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], *], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = deriving[Inj[Prod[F], *], F].divide
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]] = deriving[Inj[Prod[F], *], F].choose
    implicit def injProdToVecCopEv[F[_]]: Inj[Vector[Cop[F]], Prod[F]] = deriving[Inj[Vector[Cop[F]], *], F].divide
  }

}

object AndXor8 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8]: AndXor8[A1, A2, A3, A4, A5, A6, A7, A8] =
    new AndXor8[A1, A2, A3, A4, A5, A6, A7, A8] {}
}
