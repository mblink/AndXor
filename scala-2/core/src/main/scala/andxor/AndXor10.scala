package andxor

import andxor.types._
import andxor.syntax.ffunctor._
import andxor.syntax.ftraverse._
import cats.syntax.either._
import cats.{Applicative, Apply, Functor, Id, MonoidK, ~>}

trait AndXorNested10[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]]] extends AndXor {

  def *:[B](@annotation.unused a: AndXor1[B]): AndXorNested11[FConst[B]#T, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = AndXorNested11[FConst[B]#T, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]
  def *:[B[_[_]]](@annotation.unused a: AndXorNested1[B]): AndXorNested11[B, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = AndXorNested11[B, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

  def apply[B1]: AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T] = AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T]
  def nest[B1[_[_]]]: AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1] = AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1]

  def apply[B1, B2]: AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T] = AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2] = AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2]

  def apply[B1, B2, B3]: AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T] = AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]]]: AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3] = AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T] = AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]]]: AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4] = AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4]

  def apply[B1, B2, B3, B4, B5]: AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T] = AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]]]: AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5] = AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5]

  def apply[B1, B2, B3, B4, B5, B6]: AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T] = AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]]]: AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6] = AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6]

  def apply[B1, B2, B3, B4, B5, B6, B7]: AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T] = AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]]]: AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7] = AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8]: AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T] = AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]]]: AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8] = AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9]: AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T] = AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]]]: AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]: AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T] = AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]]]: AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]: AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T] = AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]]]: AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] = AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]: AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T] = AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T, FConst[B6]#T, FConst[B7]#T, FConst[B8]#T, FConst[B9]#T, FConst[B10]#T, FConst[B11]#T, FConst[B12]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]]]: AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] = AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]

  type Prod[F[_]] = Prod10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]]
  object Prod {
    def apply[F[_]](p: (A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F])): Prod[F] = Prod10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]](p)
  }

  type Cop[F[_]] = Cop10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]]
  object Cop {
    def apply[F[_]](c: Either[A1[F], Either[A2[F], Either[A3[F], Either[A4[F], Either[A5[F], Either[A6[F], Either[A7[F], Either[A8[F], Either[A9[F], A10[F]]]]]]]]]]): Cop[F] = Cop10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]](c)
  }

  object instances {
    implicit def axoProd10Instance(implicit ft0: FTraverse[A1, Applicative], ft1: FTraverse[A2, Applicative], ft2: FTraverse[A3, Applicative], ft3: FTraverse[A4, Applicative], ft4: FTraverse[A5, Applicative], ft5: FTraverse[A6, Applicative], ft6: FTraverse[A7, Applicative], ft7: FTraverse[A8, Applicative], ft8: FTraverse[A9, Applicative], ft9: FTraverse[A10, Applicative]): FFunctor[Prod] with FTraverseProd[Prod] =
      new FFunctor[Prod] with FTraverseProd[Prod] {
        def map[F[_], G[_]](p: Prod10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]])(nt: F ~> G): Prod10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]] =
          Prod10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]]((ft0.map(p.t1)(nt), ft1.map(p.t2)(nt), ft2.map(p.t3)(nt), ft3.map(p.t4)(nt), ft4.map(p.t5)(nt), ft5.map(p.t6)(nt), ft6.map(p.t7)(nt), ft7.map(p.t8)(nt), ft8.map(p.t9)(nt), ft9.map(p.t10)(nt)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Prod10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]]] =
          Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].map(ft0.traverse(p.t1)(f))((i0: A1[G]) => (i1: A2[G]) => (i2: A3[G]) => (i3: A4[G]) => (i4: A5[G]) => (i5: A6[G]) => (i6: A7[G]) => (i7: A8[G]) => (i8: A9[G]) => (i9: A10[G]) => Prod10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]]((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9))))(ft1.traverse(p.t2)(f)))(ft2.traverse(p.t3)(f)))(ft3.traverse(p.t4)(f)))(ft4.traverse(p.t5)(f)))(ft5.traverse(p.t6)(f)))(ft6.traverse(p.t7)(f)))(ft7.traverse(p.t8)(f)))(ft8.traverse(p.t9)(f)))(ft9.traverse(p.t10)(f))
      }

    implicit def axoProd10FoldMap(implicit fm0: FoldMap[A1, A1], fm1: FoldMap[A2, A2], fm2: FoldMap[A3, A3], fm3: FoldMap[A4, A4], fm4: FoldMap[A5, A5], fm5: FoldMap[A6, A6], fm6: FoldMap[A7, A7], fm7: FoldMap[A8, A8], fm8: FoldMap[A9, A9], fm9: FoldMap[A10, A10]): FoldMap[Prod, Cop] =
      new FoldMap[Prod, Cop] {
        def emptyProd[F[_]](implicit PE: MonoidK[F]): Prod[F] =
          Prod((fm0.emptyProd, fm1.emptyProd, fm2.emptyProd, fm3.emptyProd, fm4.emptyProd, fm5.emptyProd, fm6.emptyProd, fm7.emptyProd, fm8.emptyProd, fm9.emptyProd))

        def unconsAll[F[_], G[_]](p: Prod10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]])(implicit U: Uncons[F, G]): (List[Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]]], Prod10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]]) = {
          val (h1, t1) = fm0.unconsAll(p.t1)
          val (h2, t2) = fm1.unconsAll(p.t2)
          val (h3, t3) = fm2.unconsAll(p.t3)
          val (h4, t4) = fm3.unconsAll(p.t4)
          val (h5, t5) = fm4.unconsAll(p.t5)
          val (h6, t6) = fm5.unconsAll(p.t6)
          val (h7, t7) = fm6.unconsAll(p.t7)
          val (h8, t8) = fm7.unconsAll(p.t8)
          val (h9, t9) = fm8.unconsAll(p.t9)
          val (h10, t10) = fm9.unconsAll(p.t10)
          (
            List(h1.map(Inj[Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]], A1[G]].apply(_)), h2.map(Inj[Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]], A2[G]].apply(_)), h3.map(Inj[Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]], A3[G]].apply(_)), h4.map(Inj[Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]], A4[G]].apply(_)), h5.map(Inj[Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]], A5[G]].apply(_)), h6.map(Inj[Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]], A6[G]].apply(_)), h7.map(Inj[Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]], A7[G]].apply(_)), h8.map(Inj[Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]], A8[G]].apply(_)), h9.map(Inj[Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]], A9[G]].apply(_)), h10.map(Inj[Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]], A10[G]].apply(_))).flatten,
            Prod10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]]((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)))
        }

        def unconsOne[F[_], G[_]](p: Prod10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]], c: Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]])(implicit U: Uncons[F, G]): (Option[Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]]], Prod10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]]) =
          c.run match {

            case Left(x) =>
              val (h, t) = fm0.unconsOne(p.t1, x)
              (h.map(v => Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]](Left(v))), Prod10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]]((t, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10)))

            case Right(Left(x)) =>
              val (h, t) = fm1.unconsOne(p.t2, x)
              (h.map(v => Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]](Right(Left(v)))), Prod10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]]((p.t1, t, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10)))

            case Right(Right(Left(x))) =>
              val (h, t) = fm2.unconsOne(p.t3, x)
              (h.map(v => Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]](Right(Right(Left(v))))), Prod10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]]((p.t1, p.t2, t, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10)))

            case Right(Right(Right(Left(x)))) =>
              val (h, t) = fm3.unconsOne(p.t4, x)
              (h.map(v => Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]](Right(Right(Right(Left(v)))))), Prod10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]]((p.t1, p.t2, p.t3, t, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10)))

            case Right(Right(Right(Right(Left(x))))) =>
              val (h, t) = fm4.unconsOne(p.t5, x)
              (h.map(v => Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]](Right(Right(Right(Right(Left(v))))))), Prod10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]]((p.t1, p.t2, p.t3, p.t4, t, p.t6, p.t7, p.t8, p.t9, p.t10)))

            case Right(Right(Right(Right(Right(Left(x)))))) =>
              val (h, t) = fm5.unconsOne(p.t6, x)
              (h.map(v => Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]](Right(Right(Right(Right(Right(Left(v)))))))), Prod10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, t, p.t7, p.t8, p.t9, p.t10)))

            case Right(Right(Right(Right(Right(Right(Left(x))))))) =>
              val (h, t) = fm6.unconsOne(p.t7, x)
              (h.map(v => Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]](Right(Right(Right(Right(Right(Right(Left(v))))))))), Prod10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, t, p.t8, p.t9, p.t10)))

            case Right(Right(Right(Right(Right(Right(Right(Left(x)))))))) =>
              val (h, t) = fm7.unconsOne(p.t8, x)
              (h.map(v => Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]](Right(Right(Right(Right(Right(Right(Right(Left(v)))))))))), Prod10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, t, p.t9, p.t10)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))) =>
              val (h, t) = fm8.unconsOne(p.t9, x)
              (h.map(v => Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]](Right(Right(Right(Right(Right(Right(Right(Right(Left(v))))))))))), Prod10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, t, p.t10)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(x))))))))) =>
              val (h, t) = fm9.unconsOne(p.t10, x)
              (h.map(v => Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]](Right(Right(Right(Right(Right(Right(Right(Right(Right(v))))))))))), Prod10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, t)))

          }
      }

    implicit def axoCop10Instance(implicit ft0: FTraverse[A1, Functor], ft1: FTraverse[A2, Functor], ft2: FTraverse[A3, Functor], ft3: FTraverse[A4, Functor], ft4: FTraverse[A5, Functor], ft5: FTraverse[A6, Functor], ft6: FTraverse[A7, Functor], ft7: FTraverse[A8, Functor], ft8: FTraverse[A9, Functor], ft9: FTraverse[A10, Functor]): FFunctor[Cop] with FTraverseCop[Cop] =
      new FFunctor[Cop] with FTraverseCop[Cop] {
        def map[F[_], G[_]](c: Cop10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]])(nt: F ~> G): Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]] =
          Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]](c.run.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.map(nt)))))))))))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]]] =
          c.run match {

            case Left(x) => Functor[A].map(x.traverse(f))(y => Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]](Left(y)))

            case Right(Left(x)) => Functor[A].map(x.traverse(f))(y => Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]](Right(Left(y))))

            case Right(Right(Left(x))) => Functor[A].map(x.traverse(f))(y => Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]](Right(Right(Left(y)))))

            case Right(Right(Right(Left(x)))) => Functor[A].map(x.traverse(f))(y => Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]](Right(Right(Right(Left(y))))))

            case Right(Right(Right(Right(Left(x))))) => Functor[A].map(x.traverse(f))(y => Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]](Right(Right(Right(Right(Left(y)))))))

            case Right(Right(Right(Right(Right(Left(x)))))) => Functor[A].map(x.traverse(f))(y => Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]](Right(Right(Right(Right(Right(Left(y))))))))

            case Right(Right(Right(Right(Right(Right(Left(x))))))) => Functor[A].map(x.traverse(f))(y => Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]](Right(Right(Right(Right(Right(Right(Left(y)))))))))

            case Right(Right(Right(Right(Right(Right(Right(Left(x)))))))) => Functor[A].map(x.traverse(f))(y => Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]](Right(Right(Right(Right(Right(Right(Right(Left(y))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))) => Functor[A].map(x.traverse(f))(y => Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]](Right(Right(Right(Right(Right(Right(Right(Right(Left(y)))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(x))))))))) => Functor[A].map(x.traverse(f))(y => Cop10[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G]](Right(Right(Right(Right(Right(Right(Right(Right(Right(y)))))))))))

          }
      }
  }

  def deriving[TC[_], F[_]](implicit t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose10(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly10(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9)(x => f(Cop10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide10(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply10(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9) {
          case (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9) =>
            f(Prod10[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F]]((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1[Id]], t1: TC[A2[Id]], t2: TC[A3[Id]], t3: TC[A4[Id]], t4: TC[A5[Id]], t5: TC[A6[Id]], t6: TC[A7[Id]], t7: TC[A8[Id]], t8: TC[A9[Id]], t9: TC[A10[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]
}

object AndXorNested10 {
  def apply[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]]]: AndXorNested10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] =
    new AndXorNested10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {}
}

trait AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] extends AndXor {

  def *:[B](@annotation.unused a: AndXor1[B]): AndXor11[B, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = AndXor11[B, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]
  def *:[B[_[_]]](@annotation.unused a: AndXorNested1[B]): AndXorNested11[B, FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T] = AndXorNested11[B, FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T]

  def apply[B1]: AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1] = AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1]
  def nest[B1[_[_]]]: AndXorNested11[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1] = AndXorNested11[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1]

  def apply[B1, B2]: AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2] = AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested12[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2] = AndXorNested12[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2]

  def apply[B1, B2, B3]: AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3] = AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]]]: AndXorNested13[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2, B3] = AndXorNested13[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4] = AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]]]: AndXorNested14[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2, B3, B4] = AndXorNested14[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2, B3, B4]

  def apply[B1, B2, B3, B4, B5]: AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5] = AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]]]: AndXorNested15[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2, B3, B4, B5] = AndXorNested15[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2, B3, B4, B5]

  def apply[B1, B2, B3, B4, B5, B6]: AndXor16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6] = AndXor16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]]]: AndXorNested16[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2, B3, B4, B5, B6] = AndXorNested16[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2, B3, B4, B5, B6]

  def apply[B1, B2, B3, B4, B5, B6, B7]: AndXor17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7] = AndXor17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]]]: AndXorNested17[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2, B3, B4, B5, B6, B7] = AndXorNested17[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2, B3, B4, B5, B6, B7]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8]: AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8] = AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]]]: AndXorNested18[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2, B3, B4, B5, B6, B7, B8] = AndXorNested18[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2, B3, B4, B5, B6, B7, B8]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9]: AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]]]: AndXorNested19[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9] = AndXorNested19[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]: AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]]]: AndXorNested20[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10] = AndXorNested20[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]: AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] = AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]]]: AndXorNested21[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11] = AndXorNested21[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11]

  def apply[B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]: AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] = AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]], B6[_[_]], B7[_[_]], B8[_[_]], B9[_[_]], B10[_[_]], B11[_[_]], B12[_[_]]]: AndXorNested22[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12] = AndXorNested22[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12]

  type Prod[F[_]] = Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]
  object Prod {
    def apply[F[_]](p: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10])): Prod[F] = Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](p)
  }

  type Cop[F[_]] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]
  object Cop {
    def apply[F[_]](c: Either[F[A1], Either[F[A2], Either[F[A3], Either[F[A4], Either[F[A5], Either[F[A6], Either[F[A7], Either[F[A8], Either[F[A9], F[A10]]]]]]]]]]): Cop[F] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](c)
  }

  def deriving[TC[_], F[_]](implicit t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose10(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly10(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9)(x => f(Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide10(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply10(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9) {
          case (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9) =>
            f(Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1], t1: TC[A2], t2: TC[A3], t3: TC[A4], t4: TC[A5], t5: TC[A6], t6: TC[A7], t7: TC[A8], t8: TC[A9], t9: TC[A10]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]
}

object AndXor10 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] =
    new AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {}
}
