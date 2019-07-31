package andxor

import andxor.syntax.ffunctor._
import andxor.syntax.ftraverse._
import andxor.types._
import scalaz.{~>, \/, -\/, \/-, Applicative, Functor, Apply, Monoid}
import scalaz.Id.Id
import scalaz.std.vector._

trait AndXorNested20[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]]] extends AndXor {

  def apply[B1]: AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, FConst[B1]#T] = AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, FConst[B1]#T]
  def nest[B1[_[_]]]: AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B1] = AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B1]

  def apply[B1, B2]: AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, FConst[B1]#T, FConst[B2]#T] = AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, FConst[B1]#T, FConst[B2]#T]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B1, B2] = AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B1, B2]

  type Prod[F[_]] = Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]
  object Prod {
    def apply[F[_]](p: (A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F])): Prod[F] = Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]](p)
  }

  type Cop[F[_]] = Cop20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]
  object Cop {
    def apply[F[_]](c: (A1[F] \/ (A2[F] \/ (A3[F] \/ (A4[F] \/ (A5[F] \/ (A6[F] \/ (A7[F] \/ (A8[F] \/ (A9[F] \/ (A10[F] \/ (A11[F] \/ (A12[F] \/ (A13[F] \/ (A14[F] \/ (A15[F] \/ (A16[F] \/ (A17[F] \/ (A18[F] \/ (A19[F] \/ A20[F])))))))))))))))))))): Cop[F] = Cop20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]](c)
  }

  object instances {
    implicit def axoProd20Instance(implicit ft0: FTraverse[A1, Applicative], ft1: FTraverse[A2, Applicative], ft2: FTraverse[A3, Applicative], ft3: FTraverse[A4, Applicative], ft4: FTraverse[A5, Applicative], ft5: FTraverse[A6, Applicative], ft6: FTraverse[A7, Applicative], ft7: FTraverse[A8, Applicative], ft8: FTraverse[A9, Applicative], ft9: FTraverse[A10, Applicative], ft10: FTraverse[A11, Applicative], ft11: FTraverse[A12, Applicative], ft12: FTraverse[A13, Applicative], ft13: FTraverse[A14, Applicative], ft14: FTraverse[A15, Applicative], ft15: FTraverse[A16, Applicative], ft16: FTraverse[A17, Applicative], ft17: FTraverse[A18, Applicative], ft18: FTraverse[A19, Applicative], ft19: FTraverse[A20, Applicative]): FFunctor[Prod] with FTraverseProd[Prod] =
      new FFunctor[Prod] with FTraverseProd[Prod] {
        def map[F[_], G[_]](p: Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]])(nt: F ~> G): Prod20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]] =
          Prod20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]]((ft0.map(p.t1)(nt), ft1.map(p.t2)(nt), ft2.map(p.t3)(nt), ft3.map(p.t4)(nt), ft4.map(p.t5)(nt), ft5.map(p.t6)(nt), ft6.map(p.t7)(nt), ft7.map(p.t8)(nt), ft8.map(p.t9)(nt), ft9.map(p.t10)(nt), ft10.map(p.t11)(nt), ft11.map(p.t12)(nt), ft12.map(p.t13)(nt), ft13.map(p.t14)(nt), ft14.map(p.t15)(nt), ft15.map(p.t16)(nt), ft16.map(p.t17)(nt), ft17.map(p.t18)(nt), ft18.map(p.t19)(nt), ft19.map(p.t20)(nt)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Prod20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]]] =
          Applicative[A].ap(ft19.traverse(p.t20)(f))(Applicative[A].ap(ft18.traverse(p.t19)(f))(Applicative[A].ap(ft17.traverse(p.t18)(f))(Applicative[A].ap(ft16.traverse(p.t17)(f))(Applicative[A].ap(ft15.traverse(p.t16)(f))(Applicative[A].ap(ft14.traverse(p.t15)(f))(Applicative[A].ap(ft13.traverse(p.t14)(f))(Applicative[A].ap(ft12.traverse(p.t13)(f))(Applicative[A].ap(ft11.traverse(p.t12)(f))(Applicative[A].ap(ft10.traverse(p.t11)(f))(Applicative[A].ap(ft9.traverse(p.t10)(f))(Applicative[A].ap(ft8.traverse(p.t9)(f))(Applicative[A].ap(ft7.traverse(p.t8)(f))(Applicative[A].ap(ft6.traverse(p.t7)(f))(Applicative[A].ap(ft5.traverse(p.t6)(f))(Applicative[A].ap(ft4.traverse(p.t5)(f))(Applicative[A].ap(ft3.traverse(p.t4)(f))(Applicative[A].ap(ft2.traverse(p.t3)(f))(Applicative[A].ap(ft1.traverse(p.t2)(f))(Applicative[A].map(ft0.traverse(p.t1)(f))((i0: A1[G]) => (i1: A2[G]) => (i2: A3[G]) => (i3: A4[G]) => (i4: A5[G]) => (i5: A6[G]) => (i6: A7[G]) => (i7: A8[G]) => (i8: A9[G]) => (i9: A10[G]) => (i10: A11[G]) => (i11: A12[G]) => (i12: A13[G]) => (i13: A14[G]) => (i14: A15[G]) => (i15: A16[G]) => (i16: A17[G]) => (i17: A18[G]) => (i18: A19[G]) => (i19: A20[G]) => Prod20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]]((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19))))))))))))))))))))))
      }

    implicit def axoProd20FoldMap(implicit fm0: FoldMap[A1, A1], fm1: FoldMap[A2, A2], fm2: FoldMap[A3, A3], fm3: FoldMap[A4, A4], fm4: FoldMap[A5, A5], fm5: FoldMap[A6, A6], fm6: FoldMap[A7, A7], fm7: FoldMap[A8, A8], fm8: FoldMap[A9, A9], fm9: FoldMap[A10, A10], fm10: FoldMap[A11, A11], fm11: FoldMap[A12, A12], fm12: FoldMap[A13, A13], fm13: FoldMap[A14, A14], fm14: FoldMap[A15, A15], fm15: FoldMap[A16, A16], fm16: FoldMap[A17, A17], fm17: FoldMap[A18, A18], fm18: FoldMap[A19, A19], fm19: FoldMap[A20, A20]): FoldMap[Prod, Cop] =
      new FoldMap[Prod, Cop] {
        def unconsAll[F[_], G[_]](p: Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]])(implicit U: Uncons[F, G]): (List[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]]], Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]) = {
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
          val (h19, t19) = fm18.unconsAll(p.t19)
          val (h20, t20) = fm19.unconsAll(p.t20)
          (
            List(h1.map(Inj[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]], A1[G]].apply(_)), h2.map(Inj[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]], A2[G]].apply(_)), h3.map(Inj[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]], A3[G]].apply(_)), h4.map(Inj[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]], A4[G]].apply(_)), h5.map(Inj[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]], A5[G]].apply(_)), h6.map(Inj[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]], A6[G]].apply(_)), h7.map(Inj[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]], A7[G]].apply(_)), h8.map(Inj[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]], A8[G]].apply(_)), h9.map(Inj[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]], A9[G]].apply(_)), h10.map(Inj[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]], A10[G]].apply(_)), h11.map(Inj[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]], A11[G]].apply(_)), h12.map(Inj[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]], A12[G]].apply(_)), h13.map(Inj[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]], A13[G]].apply(_)), h14.map(Inj[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]], A14[G]].apply(_)), h15.map(Inj[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]], A15[G]].apply(_)), h16.map(Inj[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]], A16[G]].apply(_)), h17.map(Inj[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]], A17[G]].apply(_)), h18.map(Inj[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]], A18[G]].apply(_)), h19.map(Inj[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]], A19[G]].apply(_)), h20.map(Inj[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]], A20[G]].apply(_))).flatten,
            Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20)))
        }

        def unconsOne[F[_], G[_]](p: Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]], c: Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]])(implicit U: Uncons[F, G]): (Option[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]]], Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]) =
          c.run match {

            case -\/(x) =>
              val (h, t) = fm0.unconsOne(p.t1, x)
              (h.map(v => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](-\/(v))), Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((t, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18, p.t19, p.t20)))

            case \/-(-\/(x)) =>
              val (h, t) = fm1.unconsOne(p.t2, x)
              (h.map(v => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(-\/(v)))), Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((p.t1, t, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18, p.t19, p.t20)))

            case \/-(\/-(-\/(x))) =>
              val (h, t) = fm2.unconsOne(p.t3, x)
              (h.map(v => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(-\/(v))))), Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((p.t1, p.t2, t, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18, p.t19, p.t20)))

            case \/-(\/-(\/-(-\/(x)))) =>
              val (h, t) = fm3.unconsOne(p.t4, x)
              (h.map(v => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(-\/(v)))))), Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((p.t1, p.t2, p.t3, t, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18, p.t19, p.t20)))

            case \/-(\/-(\/-(\/-(-\/(x))))) =>
              val (h, t) = fm4.unconsOne(p.t5, x)
              (h.map(v => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(-\/(v))))))), Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((p.t1, p.t2, p.t3, p.t4, t, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18, p.t19, p.t20)))

            case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) =>
              val (h, t) = fm5.unconsOne(p.t6, x)
              (h.map(v => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(-\/(v)))))))), Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, t, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18, p.t19, p.t20)))

            case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) =>
              val (h, t) = fm6.unconsOne(p.t7, x)
              (h.map(v => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(-\/(v))))))))), Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, t, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18, p.t19, p.t20)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) =>
              val (h, t) = fm7.unconsOne(p.t8, x)
              (h.map(v => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v)))))))))), Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, t, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18, p.t19, p.t20)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) =>
              val (h, t) = fm8.unconsOne(p.t9, x)
              (h.map(v => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v))))))))))), Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, t, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18, p.t19, p.t20)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) =>
              val (h, t) = fm9.unconsOne(p.t10, x)
              (h.map(v => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v)))))))))))), Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, t, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18, p.t19, p.t20)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) =>
              val (h, t) = fm10.unconsOne(p.t11, x)
              (h.map(v => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v))))))))))))), Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, t, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18, p.t19, p.t20)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) =>
              val (h, t) = fm11.unconsOne(p.t12, x)
              (h.map(v => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v)))))))))))))), Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, t, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18, p.t19, p.t20)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) =>
              val (h, t) = fm12.unconsOne(p.t13, x)
              (h.map(v => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v))))))))))))))), Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, t, p.t14, p.t15, p.t16, p.t17, p.t18, p.t19, p.t20)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) =>
              val (h, t) = fm13.unconsOne(p.t14, x)
              (h.map(v => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v)))))))))))))))), Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, t, p.t15, p.t16, p.t17, p.t18, p.t19, p.t20)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) =>
              val (h, t) = fm14.unconsOne(p.t15, x)
              (h.map(v => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v))))))))))))))))), Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, t, p.t16, p.t17, p.t18, p.t19, p.t20)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) =>
              val (h, t) = fm15.unconsOne(p.t16, x)
              (h.map(v => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v)))))))))))))))))), Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, t, p.t17, p.t18, p.t19, p.t20)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))) =>
              val (h, t) = fm16.unconsOne(p.t17, x)
              (h.map(v => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v))))))))))))))))))), Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, t, p.t18, p.t19, p.t20)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))) =>
              val (h, t) = fm17.unconsOne(p.t18, x)
              (h.map(v => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v)))))))))))))))))))), Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, t, p.t19, p.t20)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))) =>
              val (h, t) = fm18.unconsOne(p.t19, x)
              (h.map(v => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v))))))))))))))))))))), Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18, t, p.t20)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))))))) =>
              val (h, t) = fm19.unconsOne(p.t20, x)
              (h.map(v => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(v))))))))))))))))))))), Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17, p.t18, p.t19, t)))

          }
      }

    implicit def axoCop20Instance(implicit ft0: FTraverse[A1, Functor], ft1: FTraverse[A2, Functor], ft2: FTraverse[A3, Functor], ft3: FTraverse[A4, Functor], ft4: FTraverse[A5, Functor], ft5: FTraverse[A6, Functor], ft6: FTraverse[A7, Functor], ft7: FTraverse[A8, Functor], ft8: FTraverse[A9, Functor], ft9: FTraverse[A10, Functor], ft10: FTraverse[A11, Functor], ft11: FTraverse[A12, Functor], ft12: FTraverse[A13, Functor], ft13: FTraverse[A14, Functor], ft14: FTraverse[A15, Functor], ft15: FTraverse[A16, Functor], ft16: FTraverse[A17, Functor], ft17: FTraverse[A18, Functor], ft18: FTraverse[A19, Functor], ft19: FTraverse[A20, Functor]): FFunctor[Cop] with FTraverseCop[Cop] =
      new FFunctor[Cop] with FTraverseCop[Cop] {
        def map[F[_], G[_]](c: Cop20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]])(nt: F ~> G): Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]] =
          Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](c.run.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.map(nt)))))))))))))))))))))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]]] =
          c.run match {

            case -\/(x) => Functor[A].map(x.traverse(f))(y => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](-\/(y)))

            case \/-(-\/(x)) => Functor[A].map(x.traverse(f))(y => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(-\/(y))))

            case \/-(\/-(-\/(x))) => Functor[A].map(x.traverse(f))(y => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(-\/(y)))))

            case \/-(\/-(\/-(-\/(x)))) => Functor[A].map(x.traverse(f))(y => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(-\/(y))))))

            case \/-(\/-(\/-(\/-(-\/(x))))) => Functor[A].map(x.traverse(f))(y => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(-\/(y)))))))

            case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Functor[A].map(x.traverse(f))(y => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(-\/(y))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Functor[A].map(x.traverse(f))(y => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Functor[A].map(x.traverse(f))(y => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Functor[A].map(x.traverse(f))(y => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Functor[A].map(x.traverse(f))(y => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Functor[A].map(x.traverse(f))(y => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Functor[A].map(x.traverse(f))(y => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Functor[A].map(x.traverse(f))(y => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => Functor[A].map(x.traverse(f))(y => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => Functor[A].map(x.traverse(f))(y => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) => Functor[A].map(x.traverse(f))(y => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))) => Functor[A].map(x.traverse(f))(y => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))) => Functor[A].map(x.traverse(f))(y => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))) => Functor[A].map(x.traverse(f))(y => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))))))) => Functor[A].map(x.traverse(f))(y => Cop20[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G], A7[G], A8[G], A9[G], A10[G], A11[G], A12[G], A13[G], A14[G], A15[G], A16[G], A17[G], A18[G], A19[G], A20[G]](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(y)))))))))))))))))))))

          }
      }
  }

  def deriving[TC[_], F[_]](implicit t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose20(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly20(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19)(x => f(Cop20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide20(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply20(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) {
          case (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19) =>
            f(Prod20[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F], A7[F], A8[F], A9[F], A10[F], A11[F], A12[F], A13[F], A14[F], A15[F], A16[F], A17[F], A18[F], A19[F], A20[F]]((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1[Id]], t1: TC[A2[Id]], t2: TC[A3[Id]], t3: TC[A4[Id]], t4: TC[A5[Id]], t5: TC[A6[Id]], t6: TC[A7[Id]], t7: TC[A8[Id]], t8: TC[A9[Id]], t9: TC[A10[Id]], t10: TC[A11[Id]], t11: TC[A12[Id]], t12: TC[A13[Id]], t13: TC[A14[Id]], t14: TC[A15[Id]], t15: TC[A16[Id]], t16: TC[A17[Id]], t17: TC[A18[Id]], t18: TC[A19[Id]], t19: TC[A20[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = deriving[Inj[Prod[F], ?], F].divide
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]] = deriving[Inj[Prod[F], ?], F].choose
    implicit def injProdToVecCopEv[F[_]]: Inj[Vector[Cop[F]], Prod[F]] = deriving[Inj[Vector[Cop[F]], ?], F].divide
  }

}

object AndXorNested20 {
  def apply[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]]]: AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
    new AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {}
}

trait AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] extends AndXor {

  def apply[B1]: AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B1] = AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B1]
  def nest[B1[_[_]]]: AndXorNested21[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T, FConst[A15]#T, FConst[A16]#T, FConst[A17]#T, FConst[A18]#T, FConst[A19]#T, FConst[A20]#T, B1] = AndXorNested21[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T, FConst[A15]#T, FConst[A16]#T, FConst[A17]#T, FConst[A18]#T, FConst[A19]#T, FConst[A20]#T, B1]

  def apply[B1, B2]: AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B1, B2] = AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B1, B2]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested22[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T, FConst[A15]#T, FConst[A16]#T, FConst[A17]#T, FConst[A18]#T, FConst[A19]#T, FConst[A20]#T, B1, B2] = AndXorNested22[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T, FConst[A15]#T, FConst[A16]#T, FConst[A17]#T, FConst[A18]#T, FConst[A19]#T, FConst[A20]#T, B1, B2]

  type Prod[F[_]] = Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
  object Prod {
    def apply[F[_]](p: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19], F[A20])): Prod[F] = Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](p)
  }

  type Cop[F[_]] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
  object Cop {
    def apply[F[_]](c: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[A19] \/ F[A20])))))))))))))))))))): Cop[F] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](c)
  }

  def deriving[TC[_], F[_]](implicit t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose20(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly20(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19)(x => f(Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide20(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply20(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) {
          case (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19) =>
            f(Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1], t1: TC[A2], t2: TC[A3], t3: TC[A4], t4: TC[A5], t5: TC[A6], t6: TC[A7], t7: TC[A8], t8: TC[A9], t9: TC[A10], t10: TC[A11], t11: TC[A12], t12: TC[A13], t13: TC[A14], t14: TC[A15], t15: TC[A16], t16: TC[A17], t17: TC[A18], t18: TC[A19], t19: TC[A20]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = deriving[Inj[Prod[F], ?], F].divide
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]] = deriving[Inj[Prod[F], ?], F].choose
    implicit def injProdToVecCopEv[F[_]]: Inj[Vector[Cop[F]], Prod[F]] = deriving[Inj[Vector[Cop[F]], ?], F].divide
  }

}

object AndXor20 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
    new AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {}
}
