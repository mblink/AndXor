package andxor

import andxor.types._
import andxor.syntax.ffunctor._
import andxor.syntax.ftraverse._
import cats.syntax.either._
import cats.{Applicative, Apply, Functor, Id, MonoidK, ~>}

trait AndXorNested18[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]]] extends AndXor {

  def *:[B](@annotation.unused a: AndXor1[B]): AndXorNested19[Lambda[f[_] => f[B]], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = AndXorNested19[Lambda[f[_] => f[B]], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
  def *:[B[_[_]]](@annotation.unused a: AndXorNested1[B]): AndXorNested19[B, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = AndXorNested19[B, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

  def apply[B1]: AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Lambda[f[_] => f[B1]]] = AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Lambda[f[_] => f[B1]]]
  def nest[B1[_[_]]]: AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B1] = AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B1]

  def apply[B1, B2]: AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]]] = AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]]]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B1, B2] = AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B1, B2]

  def apply[B1, B2, B3]: AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]]] = AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]]]: AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B1, B2, B3] = AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]]] = AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Lambda[f[_] => f[B1]], Lambda[f[_] => f[B2]], Lambda[f[_] => f[B3]], Lambda[f[_] => f[B4]]]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]]]: AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B1, B2, B3, B4] = AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B1, B2, B3, B4]

  type Prod[F[_]] = Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]
  object Prod {
    def apply[F[_]](p: (A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F])): Prod[F] = Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]](p)
  }

  type Cop[F[_]] = Cop18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]
  object Cop {
    def apply[F[_]](c: Either[A1[F], Either[A2[F], Either[A3[F], Either[A4[F], Either[A5[F], Either[A6[F], Either[A7[F], Either[A8[F], Either[A9[F], Either[A10[F], Either[A11[F], Either[A12[F], Either[A13[F], Either[A14[F], Either[A15[F], Either[A16[F], Either[A17[F], A18[F]]]]]]]]]]]]]]]]]]): Cop[F] = Cop18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]](c)
  }

  object instances {
    implicit def axoProd18Instance(implicit ft0: FTraverse[A1, Applicative], ft1: FTraverse[A2, Applicative], ft2: FTraverse[A3, Applicative], ft3: FTraverse[A4, Applicative], ft4: FTraverse[A5, Applicative], ft5: FTraverse[A6, Applicative], ft6: FTraverse[A7, Applicative], ft7: FTraverse[A8, Applicative], ft8: FTraverse[A9, Applicative], ft9: FTraverse[A10, Applicative], ft10: FTraverse[A11, Applicative], ft11: FTraverse[A12, Applicative], ft12: FTraverse[A13, Applicative], ft13: FTraverse[A14, Applicative], ft14: FTraverse[A15, Applicative], ft15: FTraverse[A16, Applicative], ft16: FTraverse[A17, Applicative], ft17: FTraverse[A18, Applicative]): FFunctor[Prod] with FTraverseProd[Prod] =
      new FFunctor[Prod] with FTraverseProd[Prod] {
        def map[F[_], G[_]](p: Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]])(nt: F ~> G): Prod18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]] =
          Prod18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]]((ft0.map(p.t1)(nt), ft1.map(p.t2)(nt), ft2.map(p.t3)(nt), ft3.map(p.t4)(nt), ft4.map(p.t5)(nt), ft5.map(p.t6)(nt), ft6.map(p.t7)(nt), ft7.map(p.t8)(nt), ft8.map(p.t9)(nt), ft9.map(p.t10)(nt), ft10.map(p.t11)(nt), ft11.map(p.t12)(nt), ft12.map(p.t13)(nt), ft13.map(p.t14)(nt), ft14.map(p.t15)(nt), ft15.map(p.t16)(nt), ft16.map(p.t17)(nt), ft17.map(p.t18)(nt)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Prod18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]]] =
          Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].map(ft0.traverse(p.t1)(f))((i0: A1[G]) => (i1: A2[G]) => (i2: A3[G]) => (i3: A4[G]) => (i4: A5[G]) => (i5: A6[G]) => (i6: A7[G]) => (i7: A8[G]) => (i8: A9[G]) => (i9: A10[G]) => (i10: A11[G]) => (i11: A12[G]) => (i12: A13[G]) => (i13: A14[G]) => (i14: A15[G]) => (i15: A16[G]) => (i16: A17[G]) => (i17: A18[G]) => Prod18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]]((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17))))(ft1.traverse(p.t2)(f)))(ft2.traverse(p.t3)(f)))(ft3.traverse(p.t4)(f)))(ft4.traverse(p.t5)(f)))(ft5.traverse(p.t6)(f)))(ft6.traverse(p.t7)(f)))(ft7.traverse(p.t8)(f)))(ft8.traverse(p.t9)(f)))(ft9.traverse(p.t10)(f)))(ft10.traverse(p.t11)(f)))(ft11.traverse(p.t12)(f)))(ft12.traverse(p.t13)(f)))(ft13.traverse(p.t14)(f)))(ft14.traverse(p.t15)(f)))(ft15.traverse(p.t16)(f)))(ft16.traverse(p.t17)(f)))(ft17.traverse(p.t18)(f))
      }

    implicit def axoProd18FoldMap(implicit fm0: FoldMap[A1, A1], fm1: FoldMap[A2, A2], fm2: FoldMap[A3, A3], fm3: FoldMap[A4, A4], fm4: FoldMap[A5, A5], fm5: FoldMap[A6, A6], fm6: FoldMap[A7, A7], fm7: FoldMap[A8, A8], fm8: FoldMap[A9, A9], fm9: FoldMap[A10, A10], fm10: FoldMap[A11, A11], fm11: FoldMap[A12, A12], fm12: FoldMap[A13, A13], fm13: FoldMap[A14, A14], fm14: FoldMap[A15, A15], fm15: FoldMap[A16, A16], fm16: FoldMap[A17, A17], fm17: FoldMap[A18, A18]): FoldMap[Prod, Cop] =
      new FoldMap[Prod, Cop] {
        def emptyProd[F[_]](implicit PE: MonoidK[F]): Prod[F] =
          Prod((fm0.emptyProd, fm1.emptyProd, fm2.emptyProd, fm3.emptyProd, fm4.emptyProd, fm5.emptyProd, fm6.emptyProd, fm7.emptyProd, fm8.emptyProd, fm9.emptyProd, fm10.emptyProd, fm11.emptyProd, fm12.emptyProd, fm13.emptyProd, fm14.emptyProd, fm15.emptyProd, fm16.emptyProd, fm17.emptyProd))

        def unconsAll[F[_], G[_]](p: Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]])(implicit U: Uncons[F, G]): (List[Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]]], Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]) = {
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
          val (h11, t11) = fm10.unconsAll(p.t11)
          val (h12, t12) = fm11.unconsAll(p.t12)
          val (h13, t13) = fm12.unconsAll(p.t13)
          val (h14, t14) = fm13.unconsAll(p.t14)
          val (h15, t15) = fm14.unconsAll(p.t15)
          val (h16, t16) = fm15.unconsAll(p.t16)
          val (h17, t17) = fm16.unconsAll(p.t17)
          val (h18, t18) = fm17.unconsAll(p.t18)
          (
            List(h1.map(Inj[Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]], A1[G]].apply(_)), h2.map(Inj[Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]], A2[G]].apply(_)), h3.map(Inj[Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]], A3[G]].apply(_)), h4.map(Inj[Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]], A4[G]].apply(_)), h5.map(Inj[Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]], A5[G]].apply(_)), h6.map(Inj[Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]], A6[G]].apply(_)), h7.map(Inj[Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]], A7[G]].apply(_)), h8.map(Inj[Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]], A8[G]].apply(_)), h9.map(Inj[Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]], A9[G]].apply(_)), h10.map(Inj[Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]], A10[G]].apply(_)), h11.map(Inj[Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]], A11[G]].apply(_)), h12.map(Inj[Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]], A12[G]].apply(_)), h13.map(Inj[Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]], A13[G]].apply(_)), h14.map(Inj[Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]], A14[G]].apply(_)), h15.map(Inj[Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]], A15[G]].apply(_)), h16.map(Inj[Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]], A16[G]].apply(_)), h17.map(Inj[Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]], A17[G]].apply(_)), h18.map(Inj[Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]], A18[G]].apply(_))).flatten,
            Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18)))
        }

        def unconsOne[F[_], G[_]](p: Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]], c: Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]])(implicit U: Uncons[F, G]): (Option[Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]]], Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]) =
          c.run match {

            case Left(x) =>
              val (h, t) = fm0.unconsOne(p.t1, x)
              (h.map(v => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Left(v))), Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]((t, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18)))

            case Right(Left(x)) =>
              val (h, t) = fm1.unconsOne(p.t2, x)
              (h.map(v => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Left(v)))), Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]((p.t1, t, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18)))

            case Right(Right(Left(x))) =>
              val (h, t) = fm2.unconsOne(p.t3, x)
              (h.map(v => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Left(v))))), Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]((p.t1, p.t2, t, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18)))

            case Right(Right(Right(Left(x)))) =>
              val (h, t) = fm3.unconsOne(p.t4, x)
              (h.map(v => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Left(v)))))), Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]((p.t1, p.t2, p.t3, t, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18)))

            case Right(Right(Right(Right(Left(x))))) =>
              val (h, t) = fm4.unconsOne(p.t5, x)
              (h.map(v => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Left(v))))))), Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]((p.t1, p.t2, p.t3, p.t4, t, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18)))

            case Right(Right(Right(Right(Right(Left(x)))))) =>
              val (h, t) = fm5.unconsOne(p.t6, x)
              (h.map(v => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Left(v)))))))), Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, t, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18)))

            case Right(Right(Right(Right(Right(Right(Left(x))))))) =>
              val (h, t) = fm6.unconsOne(p.t7, x)
              (h.map(v => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Left(v))))))))), Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, t, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18)))

            case Right(Right(Right(Right(Right(Right(Right(Left(x)))))))) =>
              val (h, t) = fm7.unconsOne(p.t8, x)
              (h.map(v => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Left(v)))))))))), Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, t, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))) =>
              val (h, t) = fm8.unconsOne(p.t9, x)
              (h.map(v => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Right(Left(v))))))))))), Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, t, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))) =>
              val (h, t) = fm9.unconsOne(p.t10, x)
              (h.map(v => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(v)))))))))))), Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, t, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))) =>
              val (h, t) = fm10.unconsOne(p.t11, x)
              (h.map(v => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(v))))))))))))), Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, t, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))))) =>
              val (h, t) = fm11.unconsOne(p.t12, x)
              (h.map(v => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(v)))))))))))))), Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, t, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))))) =>
              val (h, t) = fm12.unconsOne(p.t13, x)
              (h.map(v => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(v))))))))))))))), Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, t, p.t14, p.t15, p.t16, p.t17, p.t18)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))))))) =>
              val (h, t) = fm13.unconsOne(p.t14, x)
              (h.map(v => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(v)))))))))))))))), Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, t, p.t15, p.t16, p.t17, p.t18)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))))))) =>
              val (h, t) = fm14.unconsOne(p.t15, x)
              (h.map(v => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(v))))))))))))))))), Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, t, p.t16, p.t17, p.t18)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))))))))) =>
              val (h, t) = fm15.unconsOne(p.t16, x)
              (h.map(v => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(v)))))))))))))))))), Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, t, p.t17, p.t18)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))))))))) =>
              val (h, t) = fm16.unconsOne(p.t17, x)
              (h.map(v => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(v))))))))))))))))))), Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, t, p.t18)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(x))))))))))))))))) =>
              val (h, t) = fm17.unconsOne(p.t18, x)
              (h.map(v => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(v))))))))))))))))))), Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, t)))

          }
      }

    implicit def axoCop18Instance(implicit ft0: FTraverse[A1, Functor], ft1: FTraverse[A2, Functor], ft2: FTraverse[A3, Functor], ft3: FTraverse[A4, Functor], ft4: FTraverse[A5, Functor], ft5: FTraverse[A6, Functor], ft6: FTraverse[A7, Functor], ft7: FTraverse[A8, Functor], ft8: FTraverse[A9, Functor], ft9: FTraverse[A10, Functor], ft10: FTraverse[A11, Functor], ft11: FTraverse[A12, Functor], ft12: FTraverse[A13, Functor], ft13: FTraverse[A14, Functor], ft14: FTraverse[A15, Functor], ft15: FTraverse[A16, Functor], ft16: FTraverse[A17, Functor], ft17: FTraverse[A18, Functor]): FFunctor[Cop] with FTraverseCop[Cop] =
      new FFunctor[Cop] with FTraverseCop[Cop] {
        def map[F[_], G[_]](c: Cop18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]])(nt: F ~> G): Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]] =
          Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](c.run.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.map(nt)))))))))))))))))))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]]] =
          c.run match {

            case Left(x) => Functor[A].map(x.traverse(f))(y => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Left(y)))

            case Right(Left(x)) => Functor[A].map(x.traverse(f))(y => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Left(y))))

            case Right(Right(Left(x))) => Functor[A].map(x.traverse(f))(y => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Left(y)))))

            case Right(Right(Right(Left(x)))) => Functor[A].map(x.traverse(f))(y => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Left(y))))))

            case Right(Right(Right(Right(Left(x))))) => Functor[A].map(x.traverse(f))(y => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Left(y)))))))

            case Right(Right(Right(Right(Right(Left(x)))))) => Functor[A].map(x.traverse(f))(y => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Left(y))))))))

            case Right(Right(Right(Right(Right(Right(Left(x))))))) => Functor[A].map(x.traverse(f))(y => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Left(y)))))))))

            case Right(Right(Right(Right(Right(Right(Right(Left(x)))))))) => Functor[A].map(x.traverse(f))(y => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Left(y))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))) => Functor[A].map(x.traverse(f))(y => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Right(Left(y)))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))) => Functor[A].map(x.traverse(f))(y => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(y))))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))) => Functor[A].map(x.traverse(f))(y => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(y)))))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))))) => Functor[A].map(x.traverse(f))(y => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(y))))))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))))) => Functor[A].map(x.traverse(f))(y => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(y)))))))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))))))) => Functor[A].map(x.traverse(f))(y => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(y))))))))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))))))) => Functor[A].map(x.traverse(f))(y => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(y)))))))))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))))))))) => Functor[A].map(x.traverse(f))(y => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(y))))))))))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))))))))) => Functor[A].map(x.traverse(f))(y => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(y)))))))))))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(x))))))))))))))))) => Functor[A].map(x.traverse(f))(y => Cop18[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G]](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(y)))))))))))))))))))

          }
      }
  }

  def deriving[TC[_], F[_]](implicit t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose18(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly18(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17)(x => f(Cop18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide18(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply18(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) {
          case (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17) =>
            f(Prod18[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F]]((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1[Id]], t1: TC[A2[Id]], t2: TC[A3[Id]], t3: TC[A4[Id]], t4: TC[A5[Id]], t5: TC[A6[Id]], t6: TC[A7[Id]], t7: TC[A8[Id]], t8: TC[A9[Id]], t9: TC[A10[Id]], t10: TC[A11[Id]], t11: TC[A12[Id]], t12: TC[A13[Id]], t13: TC[A14[Id]], t14: TC[A15[Id]], t15: TC[A16[Id]], t16: TC[A17[Id]], t17: TC[A18[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]
}

object AndXorNested18 {
  def apply[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]]]: AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
    new AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {}
}

trait AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] extends AndXor {

  def *:[B](@annotation.unused a: AndXor1[B]): AndXor19[B, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = AndXor19[B, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
  def *:[B[_[_]]](@annotation.unused a: AndXorNested1[B]): AndXorNested19[B, Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], Lambda[f[_] => f[A7]], Lambda[f[_] => f[A8]], Lambda[f[_] => f[A9]], Lambda[f[_] => f[A10]], Lambda[f[_] => f[A11]], Lambda[f[_] => f[A12]], Lambda[f[_] => f[A13]], Lambda[f[_] => f[A14]], Lambda[f[_] => f[A15]], Lambda[f[_] => f[A16]], Lambda[f[_] => f[A17]], Lambda[f[_] => f[A18]]] = AndXorNested19[B, Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], Lambda[f[_] => f[A7]], Lambda[f[_] => f[A8]], Lambda[f[_] => f[A9]], Lambda[f[_] => f[A10]], Lambda[f[_] => f[A11]], Lambda[f[_] => f[A12]], Lambda[f[_] => f[A13]], Lambda[f[_] => f[A14]], Lambda[f[_] => f[A15]], Lambda[f[_] => f[A16]], Lambda[f[_] => f[A17]], Lambda[f[_] => f[A18]]]

  def apply[B1]: AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B1] = AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B1]
  def nest[B1[_[_]]]: AndXorNested19[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], Lambda[f[_] => f[A7]], Lambda[f[_] => f[A8]], Lambda[f[_] => f[A9]], Lambda[f[_] => f[A10]], Lambda[f[_] => f[A11]], Lambda[f[_] => f[A12]], Lambda[f[_] => f[A13]], Lambda[f[_] => f[A14]], Lambda[f[_] => f[A15]], Lambda[f[_] => f[A16]], Lambda[f[_] => f[A17]], Lambda[f[_] => f[A18]], B1] = AndXorNested19[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], Lambda[f[_] => f[A7]], Lambda[f[_] => f[A8]], Lambda[f[_] => f[A9]], Lambda[f[_] => f[A10]], Lambda[f[_] => f[A11]], Lambda[f[_] => f[A12]], Lambda[f[_] => f[A13]], Lambda[f[_] => f[A14]], Lambda[f[_] => f[A15]], Lambda[f[_] => f[A16]], Lambda[f[_] => f[A17]], Lambda[f[_] => f[A18]], B1]

  def apply[B1, B2]: AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B1, B2] = AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B1, B2]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested20[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], Lambda[f[_] => f[A7]], Lambda[f[_] => f[A8]], Lambda[f[_] => f[A9]], Lambda[f[_] => f[A10]], Lambda[f[_] => f[A11]], Lambda[f[_] => f[A12]], Lambda[f[_] => f[A13]], Lambda[f[_] => f[A14]], Lambda[f[_] => f[A15]], Lambda[f[_] => f[A16]], Lambda[f[_] => f[A17]], Lambda[f[_] => f[A18]], B1, B2] = AndXorNested20[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], Lambda[f[_] => f[A7]], Lambda[f[_] => f[A8]], Lambda[f[_] => f[A9]], Lambda[f[_] => f[A10]], Lambda[f[_] => f[A11]], Lambda[f[_] => f[A12]], Lambda[f[_] => f[A13]], Lambda[f[_] => f[A14]], Lambda[f[_] => f[A15]], Lambda[f[_] => f[A16]], Lambda[f[_] => f[A17]], Lambda[f[_] => f[A18]], B1, B2]

  def apply[B1, B2, B3]: AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B1, B2, B3] = AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B1, B2, B3]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]]]: AndXorNested21[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], Lambda[f[_] => f[A7]], Lambda[f[_] => f[A8]], Lambda[f[_] => f[A9]], Lambda[f[_] => f[A10]], Lambda[f[_] => f[A11]], Lambda[f[_] => f[A12]], Lambda[f[_] => f[A13]], Lambda[f[_] => f[A14]], Lambda[f[_] => f[A15]], Lambda[f[_] => f[A16]], Lambda[f[_] => f[A17]], Lambda[f[_] => f[A18]], B1, B2, B3] = AndXorNested21[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], Lambda[f[_] => f[A7]], Lambda[f[_] => f[A8]], Lambda[f[_] => f[A9]], Lambda[f[_] => f[A10]], Lambda[f[_] => f[A11]], Lambda[f[_] => f[A12]], Lambda[f[_] => f[A13]], Lambda[f[_] => f[A14]], Lambda[f[_] => f[A15]], Lambda[f[_] => f[A16]], Lambda[f[_] => f[A17]], Lambda[f[_] => f[A18]], B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B1, B2, B3, B4] = AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B1, B2, B3, B4]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]]]: AndXorNested22[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], Lambda[f[_] => f[A7]], Lambda[f[_] => f[A8]], Lambda[f[_] => f[A9]], Lambda[f[_] => f[A10]], Lambda[f[_] => f[A11]], Lambda[f[_] => f[A12]], Lambda[f[_] => f[A13]], Lambda[f[_] => f[A14]], Lambda[f[_] => f[A15]], Lambda[f[_] => f[A16]], Lambda[f[_] => f[A17]], Lambda[f[_] => f[A18]], B1, B2, B3, B4] = AndXorNested22[Lambda[f[_] => f[A1]], Lambda[f[_] => f[A2]], Lambda[f[_] => f[A3]], Lambda[f[_] => f[A4]], Lambda[f[_] => f[A5]], Lambda[f[_] => f[A6]], Lambda[f[_] => f[A7]], Lambda[f[_] => f[A8]], Lambda[f[_] => f[A9]], Lambda[f[_] => f[A10]], Lambda[f[_] => f[A11]], Lambda[f[_] => f[A12]], Lambda[f[_] => f[A13]], Lambda[f[_] => f[A14]], Lambda[f[_] => f[A15]], Lambda[f[_] => f[A16]], Lambda[f[_] => f[A17]], Lambda[f[_] => f[A18]], B1, B2, B3, B4]

  type Prod[F[_]] = Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
  object Prod {
    def apply[F[_]](p: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18])): Prod[F] = Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](p)
  }

  type Cop[F[_]] = Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
  object Cop {
    def apply[F[_]](c: Either[F[A1], Either[F[A2], Either[F[A3], Either[F[A4], Either[F[A5], Either[F[A6], Either[F[A7], Either[F[A8], Either[F[A9], Either[F[A10], Either[F[A11], Either[F[A12], Either[F[A13], Either[F[A14], Either[F[A15], Either[F[A16], Either[F[A17], F[A18]]]]]]]]]]]]]]]]]]): Cop[F] = Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](c)
  }

  def deriving[TC[_], F[_]](implicit t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose18(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly18(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17)(x => f(Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide18(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply18(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) {
          case (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17) =>
            f(Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1], t1: TC[A2], t2: TC[A3], t3: TC[A4], t4: TC[A5], t5: TC[A6], t6: TC[A7], t7: TC[A8], t8: TC[A9], t9: TC[A10], t10: TC[A11], t11: TC[A12], t12: TC[A13], t13: TC[A14], t14: TC[A15], t15: TC[A16], t16: TC[A17], t17: TC[A18]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]
}

object AndXor18 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
    new AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {}
}
