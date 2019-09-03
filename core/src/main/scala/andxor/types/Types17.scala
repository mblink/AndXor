package andxor.types

import andxor._
import scalaz.{\/, -\/, \/-, ~>, Applicative, Functor, Lens, Monoid, PLens, PlusEmpty, StoreT}
import scalaz.Id.Id
import scalaz.Isomorphism.IsoSet

trait Types17 {
  @newtype case class Prod17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17])) { self =>
    def t1: F[A1] = run._1
    def t2: F[A2] = run._2
    def t3: F[A3] = run._3
    def t4: F[A4] = run._4
    def t5: F[A5] = run._5
    def t6: F[A6] = run._6
    def t7: F[A7] = run._7
    def t8: F[A8] = run._8
    def t9: F[A9] = run._9
    def t10: F[A10] = run._10
    def t11: F[A11] = run._11
    def t12: F[A12] = run._12
    def t13: F[A13] = run._13
    def t14: F[A14] = run._14
    def t15: F[A15] = run._15
    def t16: F[A16] = run._16
    def t17: F[A17] = run._17

    private def mapN = new Map17P[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17]] {}

    def map1[B](f: F[A1] => F[B]): Prod17[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Prod17[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Prod17[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Prod17[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Prod17[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Prod17[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Prod17[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Prod17[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map3[B](f: F[A3] => F[B]): Prod17[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Prod17[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.map3(run)(f))

    def mapAt[B](f: F[A3] => F[B])(implicit d: Dummy3): Prod17[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Prod17[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map4[B](f: F[A4] => F[B]): Prod17[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Prod17[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.map4(run)(f))

    def mapAt[B](f: F[A4] => F[B])(implicit d: Dummy4): Prod17[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Prod17[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map5[B](f: F[A5] => F[B]): Prod17[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Prod17[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.map5(run)(f))

    def mapAt[B](f: F[A5] => F[B])(implicit d: Dummy5): Prod17[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Prod17[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map6[B](f: F[A6] => F[B]): Prod17[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Prod17[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.map6(run)(f))

    def mapAt[B](f: F[A6] => F[B])(implicit d: Dummy6): Prod17[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Prod17[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map7[B](f: F[A7] => F[B]): Prod17[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Prod17[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.map7(run)(f))

    def mapAt[B](f: F[A7] => F[B])(implicit d: Dummy7): Prod17[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Prod17[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map8[B](f: F[A8] => F[B]): Prod17[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Prod17[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.map8(run)(f))

    def mapAt[B](f: F[A8] => F[B])(implicit d: Dummy8): Prod17[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Prod17[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map9[B](f: F[A9] => F[B]): Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17] =
      Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17](mapN.map9(run)(f))

    def mapAt[B](f: F[A9] => F[B])(implicit d: Dummy9): Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17] =
      Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map10[B](f: F[A10] => F[B]): Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17] =
      Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17](mapN.map10(run)(f))

    def mapAt[B](f: F[A10] => F[B])(implicit d: Dummy10): Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17] =
      Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map11[B](f: F[A11] => F[B]): Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17] =
      Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17](mapN.map11(run)(f))

    def mapAt[B](f: F[A11] => F[B])(implicit d: Dummy11): Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17] =
      Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map12[B](f: F[A12] => F[B]): Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17] =
      Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17](mapN.map12(run)(f))

    def mapAt[B](f: F[A12] => F[B])(implicit d: Dummy12): Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17] =
      Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map13[B](f: F[A13] => F[B]): Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17] =
      Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17](mapN.map13(run)(f))

    def mapAt[B](f: F[A13] => F[B])(implicit d: Dummy13): Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17] =
      Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map14[B](f: F[A14] => F[B]): Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17] =
      Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17](mapN.map14(run)(f))

    def mapAt[B](f: F[A14] => F[B])(implicit d: Dummy14): Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17] =
      Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17](mapN.mapAt(f)(run))

    def map15[B](f: F[A15] => F[B]): Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17] =
      Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17](mapN.map15(run)(f))

    def mapAt[B](f: F[A15] => F[B])(implicit d: Dummy15): Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17] =
      Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17](mapN.mapAt(f)(run))

    def map16[B](f: F[A16] => F[B]): Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17] =
      Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17](mapN.map16(run)(f))

    def mapAt[B](f: F[A16] => F[B])(implicit d: Dummy16): Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17] =
      Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17](mapN.mapAt(f)(run))

    def map17[B](f: F[A17] => F[B]): Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B] =
      Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B](mapN.map17(run)(f))

    def mapAt[B](f: F[A17] => F[B])(implicit d: Dummy17): Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B] =
      Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B](mapN.mapAt(f)(run))

  }

  trait Prod17LP {

    implicit def Prod17Instance[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: FFunctor[Prod17[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] with FTraverseProd[Prod17[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      new FFunctor[Prod17[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] with FTraverseProd[Prod17[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] {
        def map[F[_], G[_]](p: Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17])(nt: F ~> G): Prod17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
          Prod17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((nt(p.t1), nt(p.t2), nt(p.t3), nt(p.t4), nt(p.t5), nt(p.t6), nt(p.t7), nt(p.t8), nt(p.t9), nt(p.t10), nt(p.t11), nt(p.t12), nt(p.t13), nt(p.t14), nt(p.t15), nt(p.t16), nt(p.t17)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17])(f: F ~> Lambda[a => A[G[a]]]): A[Prod17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
          Applicative[A].ap(f(p.t17))(Applicative[A].ap(f(p.t16))(Applicative[A].ap(f(p.t15))(Applicative[A].ap(f(p.t14))(Applicative[A].ap(f(p.t13))(Applicative[A].ap(f(p.t12))(Applicative[A].ap(f(p.t11))(Applicative[A].ap(f(p.t10))(Applicative[A].ap(f(p.t9))(Applicative[A].ap(f(p.t8))(Applicative[A].ap(f(p.t7))(Applicative[A].ap(f(p.t6))(Applicative[A].ap(f(p.t5))(Applicative[A].ap(f(p.t4))(Applicative[A].ap(f(p.t3))(Applicative[A].ap(f(p.t2))(Applicative[A].map(f(p.t1))((i0: G[A1]) => (i1: G[A2]) => (i2: G[A3]) => (i3: G[A4]) => (i4: G[A5]) => (i5: G[A6]) => (i6: G[A7]) => (i7: G[A8]) => (i8: G[A9]) => (i9: G[A10]) => (i10: G[A11]) => (i11: G[A12]) => (i12: G[A13]) => (i13: G[A14]) => (i14: G[A15]) => (i15: G[A16]) => (i16: G[A17]) => Prod17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16)))))))))))))))))))
      }

    implicit def Prod17FoldMap[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: FoldMap[Prod17[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], Cop17[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      new FoldMap[Prod17[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], Cop17[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] {
        def emptyProd[F[_]](implicit PE: PlusEmpty[F]): Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
          Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5], PE.empty[A6], PE.empty[A7], PE.empty[A8], PE.empty[A9], PE.empty[A10], PE.empty[A11], PE.empty[A12], PE.empty[A13], PE.empty[A14], PE.empty[A15], PE.empty[A16], PE.empty[A17]))

        def unconsAll[F[_], G[_]](p: Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17])(implicit U: Uncons[F, G]): (List[Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]) = {
          val (h1, t1) = U(p.t1)
          val (h2, t2) = U(p.t2)
          val (h3, t3) = U(p.t3)
          val (h4, t4) = U(p.t4)
          val (h5, t5) = U(p.t5)
          val (h6, t6) = U(p.t6)
          val (h7, t7) = U(p.t7)
          val (h8, t8) = U(p.t8)
          val (h9, t9) = U(p.t9)
          val (h10, t10) = U(p.t10)
          val (h11, t11) = U(p.t11)
          val (h12, t12) = U(p.t12)
          val (h13, t13) = U(p.t13)
          val (h14, t14) = U(p.t14)
          val (h15, t15) = U(p.t15)
          val (h16, t16) = U(p.t16)
          val (h17, t17) = U(p.t17)
          (
            List(h1.map(Inj[Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], G[A1]].apply(_)), h2.map(Inj[Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], G[A2]].apply(_)), h3.map(Inj[Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], G[A3]].apply(_)), h4.map(Inj[Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], G[A4]].apply(_)), h5.map(Inj[Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], G[A5]].apply(_)), h6.map(Inj[Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], G[A6]].apply(_)), h7.map(Inj[Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], G[A7]].apply(_)), h8.map(Inj[Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], G[A8]].apply(_)), h9.map(Inj[Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], G[A9]].apply(_)), h10.map(Inj[Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], G[A10]].apply(_)), h11.map(Inj[Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], G[A11]].apply(_)), h12.map(Inj[Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], G[A12]].apply(_)), h13.map(Inj[Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], G[A13]].apply(_)), h14.map(Inj[Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], G[A14]].apply(_)), h15.map(Inj[Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], G[A15]].apply(_)), h16.map(Inj[Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], G[A16]].apply(_)), h17.map(Inj[Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], G[A17]].apply(_))).flatten,
            Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17)))
        }

        def unconsOne[F[_], G[_]](p: Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], c: Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17])(implicit U: Uncons[F, G]): (Option[Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]) =
          c.run match {

            case -\/(_) =>
              val (h, t) = U(p.t1)
              (h.map(v => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](-\/(v))), Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17)))

            case \/-(-\/(_)) =>
              val (h, t) = U(p.t2)
              (h.map(v => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(-\/(v)))), Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, t, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17)))

            case \/-(\/-(-\/(_))) =>
              val (h, t) = U(p.t3)
              (h.map(v => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(-\/(v))))), Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, t, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17)))

            case \/-(\/-(\/-(-\/(_)))) =>
              val (h, t) = U(p.t4)
              (h.map(v => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(-\/(v)))))), Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, t, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17)))

            case \/-(\/-(\/-(\/-(-\/(_))))) =>
              val (h, t) = U(p.t5)
              (h.map(v => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(-\/(v))))))), Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, t, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17)))

            case \/-(\/-(\/-(\/-(\/-(-\/(_)))))) =>
              val (h, t) = U(p.t6)
              (h.map(v => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(-\/(v)))))))), Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, t, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17)))

            case \/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))) =>
              val (h, t) = U(p.t7)
              (h.map(v => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(-\/(v))))))))), Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, t, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))) =>
              val (h, t) = U(p.t8)
              (h.map(v => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v)))))))))), Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, t, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))) =>
              val (h, t) = U(p.t9)
              (h.map(v => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v))))))))))), Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, t, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))))) =>
              val (h, t) = U(p.t10)
              (h.map(v => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v)))))))))))), Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, t, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))))) =>
              val (h, t) = U(p.t11)
              (h.map(v => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v))))))))))))), Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, t, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))))))) =>
              val (h, t) = U(p.t12)
              (h.map(v => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v)))))))))))))), Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, t, p.t13, p.t14, p.t15, p.t16, p.t17)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))))))) =>
              val (h, t) = U(p.t13)
              (h.map(v => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v))))))))))))))), Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, t, p.t14, p.t15, p.t16, p.t17)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))))))))) =>
              val (h, t) = U(p.t14)
              (h.map(v => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v)))))))))))))))), Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, t, p.t15, p.t16, p.t17)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))))))))) =>
              val (h, t) = U(p.t15)
              (h.map(v => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v))))))))))))))))), Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, t, p.t16, p.t17)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))))))))))) =>
              val (h, t) = U(p.t16)
              (h.map(v => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v)))))))))))))))))), Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, t, p.t17)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(_)))))))))))))))) =>
              val (h, t) = U(p.t17)
              (h.map(v => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(v)))))))))))))))))), Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, t)))

          }
      }

    def Prod17TupleIso[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: IsoSet[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17])] =
      IsoSet((_: Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]).run, Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](_: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17])))

    implicit def Prod17Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17])]): Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      Monoid.fromIso(Prod17TupleIso[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17])(M)

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A1]] = {
      val t = M.zero
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((x, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13, t.t14, t.t15, t.t16, t.t17)))
    }

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A2]] = {
      val t = M.zero
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t.t1, x, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13, t.t14, t.t15, t.t16, t.t17)))
    }

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A3]] = {
      val t = M.zero
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t.t1, t.t2, x, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13, t.t14, t.t15, t.t16, t.t17)))
    }

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A4]] = {
      val t = M.zero
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t.t1, t.t2, t.t3, x, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13, t.t14, t.t15, t.t16, t.t17)))
    }

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A5]] = {
      val t = M.zero
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t.t1, t.t2, t.t3, t.t4, x, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13, t.t14, t.t15, t.t16, t.t17)))
    }

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A6]] = {
      val t = M.zero
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t.t1, t.t2, t.t3, t.t4, t.t5, x, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13, t.t14, t.t15, t.t16, t.t17)))
    }

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A7]] = {
      val t = M.zero
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, x, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13, t.t14, t.t15, t.t16, t.t17)))
    }

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A8]] = {
      val t = M.zero
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, x, t.t9, t.t10, t.t11, t.t12, t.t13, t.t14, t.t15, t.t16, t.t17)))
    }

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A9]] = {
      val t = M.zero
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, x, t.t10, t.t11, t.t12, t.t13, t.t14, t.t15, t.t16, t.t17)))
    }

    implicit def lifta9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A10]] = {
      val t = M.zero
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, x, t.t11, t.t12, t.t13, t.t14, t.t15, t.t16, t.t17)))
    }

    implicit def lifta10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A11]] = {
      val t = M.zero
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, x, t.t12, t.t13, t.t14, t.t15, t.t16, t.t17)))
    }

    implicit def lifta11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A12]] = {
      val t = M.zero
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, x, t.t13, t.t14, t.t15, t.t16, t.t17)))
    }

    implicit def lifta12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A13]] = {
      val t = M.zero
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, x, t.t14, t.t15, t.t16, t.t17)))
    }

    implicit def lifta13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A14]] = {
      val t = M.zero
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13, x, t.t15, t.t16, t.t17)))
    }

    implicit def lifta14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A15]] = {
      val t = M.zero
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13, t.t14, x, t.t16, t.t17)))
    }

    implicit def lifta15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A16]] = {
      val t = M.zero
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13, t.t14, t.t15, x, t.t17)))
    }

    implicit def lifta16F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A17]] = {
      val t = M.zero
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13, t.t14, t.t15, t.t16, x)))
    }

    implicit def Prod17Lens0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A1]] =
      Lens(p => StoreT.store[F[A1], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](p.t1)(x =>
        Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((x, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17))))

    implicit def Prod17Lens1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A2]] =
      Lens(p => StoreT.store[F[A2], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](p.t2)(x =>
        Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, x, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17))))

    implicit def Prod17Lens2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A3]] =
      Lens(p => StoreT.store[F[A3], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](p.t3)(x =>
        Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, x, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17))))

    implicit def Prod17Lens3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A4]] =
      Lens(p => StoreT.store[F[A4], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](p.t4)(x =>
        Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, x, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17))))

    implicit def Prod17Lens4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A5]] =
      Lens(p => StoreT.store[F[A5], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](p.t5)(x =>
        Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, x, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17))))

    implicit def Prod17Lens5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A6]] =
      Lens(p => StoreT.store[F[A6], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](p.t6)(x =>
        Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, x, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17))))

    implicit def Prod17Lens6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A7]] =
      Lens(p => StoreT.store[F[A7], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](p.t7)(x =>
        Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, x, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17))))

    implicit def Prod17Lens7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A8]] =
      Lens(p => StoreT.store[F[A8], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](p.t8)(x =>
        Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, x, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17))))

    implicit def Prod17Lens8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A9]] =
      Lens(p => StoreT.store[F[A9], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](p.t9)(x =>
        Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, x, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17))))

    implicit def Prod17Lens9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A10]] =
      Lens(p => StoreT.store[F[A10], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](p.t10)(x =>
        Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, x, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17))))

    implicit def Prod17Lens10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A11]] =
      Lens(p => StoreT.store[F[A11], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](p.t11)(x =>
        Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, x, p.t12, p.t13, p.t14, p.t15, p.t16, p.t17))))

    implicit def Prod17Lens11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A12]] =
      Lens(p => StoreT.store[F[A12], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](p.t12)(x =>
        Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, x, p.t13, p.t14, p.t15, p.t16, p.t17))))

    implicit def Prod17Lens12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A13]] =
      Lens(p => StoreT.store[F[A13], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](p.t13)(x =>
        Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, x, p.t14, p.t15, p.t16, p.t17))))

    implicit def Prod17Lens13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A14]] =
      Lens(p => StoreT.store[F[A14], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](p.t14)(x =>
        Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, x, p.t15, p.t16, p.t17))))

    implicit def Prod17Lens14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A15]] =
      Lens(p => StoreT.store[F[A15], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](p.t15)(x =>
        Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, x, p.t16, p.t17))))

    implicit def Prod17Lens15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A16]] =
      Lens(p => StoreT.store[F[A16], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](p.t16)(x =>
        Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, x, p.t17))))

    implicit def Prod17Lens16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A17]] =
      Lens(p => StoreT.store[F[A17], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](p.t17)(x =>
        Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14, p.t15, p.t16, x))))

  }

  object Prod17 extends Prod17LP {

    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A1] =
      lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A2] =
      lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A3] =
      lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A4] =
      lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A5] =
      lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A6] =
      lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A7] =
      lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A8] =
      lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A9] =
      lifta8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A10] =
      lifta9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A11] =
      lifta10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A12] =
      lifta11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A13] =
      lifta12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A14] =
      lifta13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A15] =
      lifta14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A16] =
      lifta15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A17] =
      lifta16F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Prod17Lens0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A1] =
      Prod17Lens0[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Prod17Lens1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A2] =
      Prod17Lens1[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Prod17Lens2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A3] =
      Prod17Lens2[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Prod17Lens3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A4] =
      Prod17Lens3[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Prod17Lens4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A5] =
      Prod17Lens4[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Prod17Lens5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A6] =
      Prod17Lens5[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Prod17Lens6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A7] =
      Prod17Lens6[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Prod17Lens7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A8] =
      Prod17Lens7[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Prod17Lens8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A9] =
      Prod17Lens8[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Prod17Lens9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A10] =
      Prod17Lens9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Prod17Lens10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A11] =
      Prod17Lens10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Prod17Lens11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A12] =
      Prod17Lens11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Prod17Lens12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A13] =
      Prod17Lens12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Prod17Lens13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A14] =
      Prod17Lens13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Prod17Lens14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A15] =
      Prod17Lens14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Prod17Lens15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A16] =
      Prod17Lens15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Prod17Lens16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Lens[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A17] =
      Prod17Lens16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

  }

  @newtype case class Cop17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ F[A17]))))))))))))))))) {
    private def mapN = new Map17C[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17]] {}

    def map1[B](f: F[A1] => F[B]): Cop17[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Cop17[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Cop17[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Cop17[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Cop17[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Cop17[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Cop17[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Cop17[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map3[B](f: F[A3] => F[B]): Cop17[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Cop17[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.map3(run)(f))

    def mapAt[B](f: F[A3] => F[B])(implicit d: Dummy3): Cop17[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Cop17[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map4[B](f: F[A4] => F[B]): Cop17[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Cop17[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.map4(run)(f))

    def mapAt[B](f: F[A4] => F[B])(implicit d: Dummy4): Cop17[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Cop17[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map5[B](f: F[A5] => F[B]): Cop17[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Cop17[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.map5(run)(f))

    def mapAt[B](f: F[A5] => F[B])(implicit d: Dummy5): Cop17[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Cop17[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map6[B](f: F[A6] => F[B]): Cop17[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Cop17[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.map6(run)(f))

    def mapAt[B](f: F[A6] => F[B])(implicit d: Dummy6): Cop17[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Cop17[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map7[B](f: F[A7] => F[B]): Cop17[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Cop17[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.map7(run)(f))

    def mapAt[B](f: F[A7] => F[B])(implicit d: Dummy7): Cop17[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Cop17[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map8[B](f: F[A8] => F[B]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Cop17[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.map8(run)(f))

    def mapAt[B](f: F[A8] => F[B])(implicit d: Dummy8): Cop17[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      Cop17[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map9[B](f: F[A9] => F[B]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17] =
      Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17](mapN.map9(run)(f))

    def mapAt[B](f: F[A9] => F[B])(implicit d: Dummy9): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17] =
      Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map10[B](f: F[A10] => F[B]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17] =
      Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17](mapN.map10(run)(f))

    def mapAt[B](f: F[A10] => F[B])(implicit d: Dummy10): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17] =
      Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map11[B](f: F[A11] => F[B]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17] =
      Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17](mapN.map11(run)(f))

    def mapAt[B](f: F[A11] => F[B])(implicit d: Dummy11): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17] =
      Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map12[B](f: F[A12] => F[B]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17] =
      Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17](mapN.map12(run)(f))

    def mapAt[B](f: F[A12] => F[B])(implicit d: Dummy12): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17] =
      Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map13[B](f: F[A13] => F[B]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17] =
      Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17](mapN.map13(run)(f))

    def mapAt[B](f: F[A13] => F[B])(implicit d: Dummy13): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17] =
      Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17](mapN.mapAt(f)(run))

    def map14[B](f: F[A14] => F[B]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17] =
      Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17](mapN.map14(run)(f))

    def mapAt[B](f: F[A14] => F[B])(implicit d: Dummy14): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17] =
      Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17](mapN.mapAt(f)(run))

    def map15[B](f: F[A15] => F[B]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17] =
      Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17](mapN.map15(run)(f))

    def mapAt[B](f: F[A15] => F[B])(implicit d: Dummy15): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17] =
      Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17](mapN.mapAt(f)(run))

    def map16[B](f: F[A16] => F[B]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17] =
      Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17](mapN.map16(run)(f))

    def mapAt[B](f: F[A16] => F[B])(implicit d: Dummy16): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17] =
      Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17](mapN.mapAt(f)(run))

    def map17[B](f: F[A17] => F[B]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B] =
      Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B](mapN.map17(run)(f))

    def mapAt[B](f: F[A17] => F[B])(implicit d: Dummy17): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B] =
      Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B](mapN.mapAt(f)(run))

  }

  trait Cop17LP {

    implicit def Cop17Instance[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: FFunctor[Cop17[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] with FTraverseCop[Cop17[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      new FFunctor[Cop17[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] with FTraverseCop[Cop17[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] {
        def map[F[_], G[_]](c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17])(nt: F ~> G): Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
          Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](c.run.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_))))))))))))))))))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17])(f: F ~> Lambda[a => A[G[a]]]): A[Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
          c.run match {

            case -\/(x) => Functor[A].map(f(x))(y => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](-\/(y)))

            case \/-(-\/(x)) => Functor[A].map(f(x))(y => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(-\/(y))))

            case \/-(\/-(-\/(x))) => Functor[A].map(f(x))(y => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(-\/(y)))))

            case \/-(\/-(\/-(-\/(x)))) => Functor[A].map(f(x))(y => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(-\/(y))))))

            case \/-(\/-(\/-(\/-(-\/(x))))) => Functor[A].map(f(x))(y => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(-\/(y)))))))

            case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Functor[A].map(f(x))(y => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(-\/(y))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Functor[A].map(f(x))(y => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Functor[A].map(f(x))(y => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Functor[A].map(f(x))(y => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Functor[A].map(f(x))(y => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Functor[A].map(f(x))(y => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Functor[A].map(f(x))(y => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Functor[A].map(f(x))(y => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => Functor[A].map(f(x))(y => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => Functor[A].map(f(x))(y => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) => Functor[A].map(f(x))(y => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))) => Functor[A].map(f(x))(y => Cop17[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(y))))))))))))))))))

          }
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A1]] =
      Inj.instance(x => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](-\/(x)))

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A2]] =
      Inj.instance(x => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(-\/(x))))

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A3]] =
      Inj.instance(x => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(-\/(x)))))

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A4]] =
      Inj.instance(x => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(-\/(x))))))

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A5]] =
      Inj.instance(x => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(-\/(x)))))))

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A6]] =
      Inj.instance(x => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(-\/(x))))))))

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A7]] =
      Inj.instance(x => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A8]] =
      Inj.instance(x => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A9]] =
      Inj.instance(x => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))

    implicit def inja9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A10]] =
      Inj.instance(x => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))

    implicit def inja10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A11]] =
      Inj.instance(x => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))

    implicit def inja11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A12]] =
      Inj.instance(x => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))

    implicit def inja12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A13]] =
      Inj.instance(x => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))

    implicit def inja13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A14]] =
      Inj.instance(x => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))

    implicit def inja14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A15]] =
      Inj.instance(x => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))

    implicit def inja15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A16]] =
      Inj.instance(x => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))

    implicit def inja16F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A17]] =
      Inj.instance(x => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))))))

    implicit def Cop17PLens0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A1]] =
      PLens(c => c.run match {
        case -\/(x) => Some(StoreT.store[F[A1], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](x)(y => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](-\/(y))))
        case _ => None
      })

    implicit def Cop17PLens1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A2]] =
      PLens(c => c.run match {
        case \/-(-\/(x)) => Some(StoreT.store[F[A2], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](x)(y => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(-\/(y)))))
        case _ => None
      })

    implicit def Cop17PLens2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A3]] =
      PLens(c => c.run match {
        case \/-(\/-(-\/(x))) => Some(StoreT.store[F[A3], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](x)(y => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(-\/(y))))))
        case _ => None
      })

    implicit def Cop17PLens3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A4]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(StoreT.store[F[A4], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](x)(y => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(-\/(y)))))))
        case _ => None
      })

    implicit def Cop17PLens4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A5]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(StoreT.store[F[A5], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](x)(y => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(-\/(y))))))))
        case _ => None
      })

    implicit def Cop17PLens5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A6]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(StoreT.store[F[A6], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](x)(y => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))
        case _ => None
      })

    implicit def Cop17PLens6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A7]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(StoreT.store[F[A7], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](x)(y => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))
        case _ => None
      })

    implicit def Cop17PLens7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A8]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(StoreT.store[F[A8], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](x)(y => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))
        case _ => None
      })

    implicit def Cop17PLens8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A9]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(StoreT.store[F[A9], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](x)(y => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))
        case _ => None
      })

    implicit def Cop17PLens9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A10]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(StoreT.store[F[A10], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](x)(y => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))
        case _ => None
      })

    implicit def Cop17PLens10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A11]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(StoreT.store[F[A11], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](x)(y => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))
        case _ => None
      })

    implicit def Cop17PLens11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A12]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(StoreT.store[F[A12], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](x)(y => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))
        case _ => None
      })

    implicit def Cop17PLens12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A13]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Some(StoreT.store[F[A13], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](x)(y => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))))
        case _ => None
      })

    implicit def Cop17PLens13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A14]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => Some(StoreT.store[F[A14], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](x)(y => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))))
        case _ => None
      })

    implicit def Cop17PLens14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A15]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => Some(StoreT.store[F[A15], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](x)(y => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))))))
        case _ => None
      })

    implicit def Cop17PLens15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A16]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) => Some(StoreT.store[F[A16], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](x)(y => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))))))
        case _ => None
      })

    implicit def Cop17PLens16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A17]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))) => Some(StoreT.store[F[A17], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](x)(y => Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(y)))))))))))))))))))
        case _ => None
      })

  }

  object Cop17 extends Cop17LP {

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A1] =
      inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A2] =
      inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A3] =
      inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A4] =
      inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A5] =
      inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A6] =
      inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A7] =
      inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A8] =
      inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A9] =
      inja8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A10] =
      inja9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A11] =
      inja10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A12] =
      inja11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A13] =
      inja12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A14] =
      inja13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A15] =
      inja14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A16] =
      inja15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A17] =
      inja16F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Cop17PLens0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A1] =
      Cop17PLens0[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Cop17PLens1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A2] =
      Cop17PLens1[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Cop17PLens2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A3] =
      Cop17PLens2[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Cop17PLens3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A4] =
      Cop17PLens3[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Cop17PLens4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A5] =
      Cop17PLens4[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Cop17PLens5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A6] =
      Cop17PLens5[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Cop17PLens6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A7] =
      Cop17PLens6[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Cop17PLens7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A8] =
      Cop17PLens7[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Cop17PLens8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A9] =
      Cop17PLens8[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Cop17PLens9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A10] =
      Cop17PLens9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Cop17PLens10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A11] =
      Cop17PLens10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Cop17PLens11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A12] =
      Cop17PLens11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Cop17PLens12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A13] =
      Cop17PLens12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Cop17PLens13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A14] =
      Cop17PLens13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Cop17PLens14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A15] =
      Cop17PLens14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Cop17PLens15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A16] =
      Cop17PLens15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def Cop17PLens16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: PLens[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A17] =
      Cop17PLens16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

  }
}
