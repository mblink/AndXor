package andxor.types

import andxor._
import monocle.{Lens, Optional}
import cats.{~>, Applicative, Functor, Id, Monoid, MonoidK}
import cats.syntax.either._
import monocle.Iso

trait Types14 {
  @newtype case class Prod14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14])) { self =>
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

    private def mapN = new Map14P[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14]] {}

    def map1[B](f: F[A1] => F[B]): Prod14[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
      Prod14[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Prod14[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
      Prod14[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Prod14[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
      Prod14[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Prod14[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
      Prod14[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](mapN.mapAt(f)(run))

    def map3[B](f: F[A3] => F[B]): Prod14[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
      Prod14[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](mapN.map3(run)(f))

    def mapAt[B](f: F[A3] => F[B])(implicit d: Dummy3): Prod14[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
      Prod14[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](mapN.mapAt(f)(run))

    def map4[B](f: F[A4] => F[B]): Prod14[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
      Prod14[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](mapN.map4(run)(f))

    def mapAt[B](f: F[A4] => F[B])(implicit d: Dummy4): Prod14[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
      Prod14[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](mapN.mapAt(f)(run))

    def map5[B](f: F[A5] => F[B]): Prod14[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
      Prod14[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14](mapN.map5(run)(f))

    def mapAt[B](f: F[A5] => F[B])(implicit d: Dummy5): Prod14[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
      Prod14[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14](mapN.mapAt(f)(run))

    def map6[B](f: F[A6] => F[B]): Prod14[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14] =
      Prod14[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14](mapN.map6(run)(f))

    def mapAt[B](f: F[A6] => F[B])(implicit d: Dummy6): Prod14[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14] =
      Prod14[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14](mapN.mapAt(f)(run))

    def map7[B](f: F[A7] => F[B]): Prod14[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14] =
      Prod14[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14](mapN.map7(run)(f))

    def mapAt[B](f: F[A7] => F[B])(implicit d: Dummy7): Prod14[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14] =
      Prod14[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14](mapN.mapAt(f)(run))

    def map8[B](f: F[A8] => F[B]): Prod14[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14] =
      Prod14[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14](mapN.map8(run)(f))

    def mapAt[B](f: F[A8] => F[B])(implicit d: Dummy8): Prod14[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14] =
      Prod14[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14](mapN.mapAt(f)(run))

    def map9[B](f: F[A9] => F[B]): Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14] =
      Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14](mapN.map9(run)(f))

    def mapAt[B](f: F[A9] => F[B])(implicit d: Dummy9): Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14] =
      Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14](mapN.mapAt(f)(run))

    def map10[B](f: F[A10] => F[B]): Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14] =
      Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14](mapN.map10(run)(f))

    def mapAt[B](f: F[A10] => F[B])(implicit d: Dummy10): Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14] =
      Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14](mapN.mapAt(f)(run))

    def map11[B](f: F[A11] => F[B]): Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14] =
      Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14](mapN.map11(run)(f))

    def mapAt[B](f: F[A11] => F[B])(implicit d: Dummy11): Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14] =
      Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14](mapN.mapAt(f)(run))

    def map12[B](f: F[A12] => F[B]): Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14] =
      Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14](mapN.map12(run)(f))

    def mapAt[B](f: F[A12] => F[B])(implicit d: Dummy12): Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14] =
      Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14](mapN.mapAt(f)(run))

    def map13[B](f: F[A13] => F[B]): Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14] =
      Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14](mapN.map13(run)(f))

    def mapAt[B](f: F[A13] => F[B])(implicit d: Dummy13): Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14] =
      Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14](mapN.mapAt(f)(run))

    def map14[B](f: F[A14] => F[B]): Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B] =
      Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B](mapN.map14(run)(f))

    def mapAt[B](f: F[A14] => F[B])(implicit d: Dummy14): Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B] =
      Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B](mapN.mapAt(f)(run))

  }

  trait Prod14LP {

    implicit def Prod14Instance[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: FFunctor[Prod14[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] with FTraverseProd[Prod14[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      new FFunctor[Prod14[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] with FTraverseProd[Prod14[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] {
        def map[F[_], G[_]](p: Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14])(nt: F ~> G): Prod14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
          Prod14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((nt(p.t1), nt(p.t2), nt(p.t3), nt(p.t4), nt(p.t5), nt(p.t6), nt(p.t7), nt(p.t8), nt(p.t9), nt(p.t10), nt(p.t11), nt(p.t12), nt(p.t13), nt(p.t14)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14])(f: F ~> Lambda[a => A[G[a]]]): A[Prod14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
          Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].map(f(p.t1))((i0: G[A1]) => (i1: G[A2]) => (i2: G[A3]) => (i3: G[A4]) => (i4: G[A5]) => (i5: G[A6]) => (i6: G[A7]) => (i7: G[A8]) => (i8: G[A9]) => (i9: G[A10]) => (i10: G[A11]) => (i11: G[A12]) => (i12: G[A13]) => (i13: G[A14]) => Prod14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13))))(f(p.t2)))(f(p.t3)))(f(p.t4)))(f(p.t5)))(f(p.t6)))(f(p.t7)))(f(p.t8)))(f(p.t9)))(f(p.t10)))(f(p.t11)))(f(p.t12)))(f(p.t13)))(f(p.t14))
      }

    implicit def Prod14FoldMap[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: FoldMap[Prod14[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], Cop14[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      new FoldMap[Prod14[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], Cop14[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] {
        def emptyProd[F[_]](implicit PE: MonoidK[F]): Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
          Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5], PE.empty[A6], PE.empty[A7], PE.empty[A8], PE.empty[A9], PE.empty[A10], PE.empty[A11], PE.empty[A12], PE.empty[A13], PE.empty[A14]))

        def unconsAll[F[_], G[_]](p: Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14])(implicit U: Uncons[F, G]): (List[Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]) = {
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
          (
            List(h1.map(Inj[Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], G[A1]].apply(_)), h2.map(Inj[Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], G[A2]].apply(_)), h3.map(Inj[Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], G[A3]].apply(_)), h4.map(Inj[Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], G[A4]].apply(_)), h5.map(Inj[Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], G[A5]].apply(_)), h6.map(Inj[Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], G[A6]].apply(_)), h7.map(Inj[Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], G[A7]].apply(_)), h8.map(Inj[Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], G[A8]].apply(_)), h9.map(Inj[Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], G[A9]].apply(_)), h10.map(Inj[Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], G[A10]].apply(_)), h11.map(Inj[Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], G[A11]].apply(_)), h12.map(Inj[Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], G[A12]].apply(_)), h13.map(Inj[Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], G[A13]].apply(_)), h14.map(Inj[Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], G[A14]].apply(_))).flatten,
            Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)))
        }

        def unconsOne[F[_], G[_]](p: Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], c: Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14])(implicit U: Uncons[F, G]): (Option[Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]) =
          c.run match {

            case Left(_) =>
              val (h, t) = U(p.t1)
              (h.map(v => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Left(v))), Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14)))

            case Right(Left(_)) =>
              val (h, t) = U(p.t2)
              (h.map(v => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Left(v)))), Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, t, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14)))

            case Right(Right(Left(_))) =>
              val (h, t) = U(p.t3)
              (h.map(v => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Left(v))))), Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, t, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14)))

            case Right(Right(Right(Left(_)))) =>
              val (h, t) = U(p.t4)
              (h.map(v => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Left(v)))))), Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, t, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14)))

            case Right(Right(Right(Right(Left(_))))) =>
              val (h, t) = U(p.t5)
              (h.map(v => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Left(v))))))), Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, p.t4, t, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14)))

            case Right(Right(Right(Right(Right(Left(_)))))) =>
              val (h, t) = U(p.t6)
              (h.map(v => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Left(v)))))))), Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, p.t4, p.t5, t, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14)))

            case Right(Right(Right(Right(Right(Right(Left(_))))))) =>
              val (h, t) = U(p.t7)
              (h.map(v => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Left(v))))))))), Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, t, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14)))

            case Right(Right(Right(Right(Right(Right(Right(Left(_)))))))) =>
              val (h, t) = U(p.t8)
              (h.map(v => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Left(v)))))))))), Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, t, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Left(_))))))))) =>
              val (h, t) = U(p.t9)
              (h.map(v => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Left(v))))))))))), Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, t, p.t10, p.t11, p.t12, p.t13, p.t14)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(_)))))))))) =>
              val (h, t) = U(p.t10)
              (h.map(v => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(v)))))))))))), Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, t, p.t11, p.t12, p.t13, p.t14)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(_))))))))))) =>
              val (h, t) = U(p.t11)
              (h.map(v => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(v))))))))))))), Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, t, p.t12, p.t13, p.t14)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(_)))))))))))) =>
              val (h, t) = U(p.t12)
              (h.map(v => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(v)))))))))))))), Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, t, p.t13, p.t14)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(_))))))))))))) =>
              val (h, t) = U(p.t13)
              (h.map(v => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(v))))))))))))))), Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, t, p.t14)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(_))))))))))))) =>
              val (h, t) = U(p.t14)
              (h.map(v => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(v))))))))))))))), Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, t)))

          }
      }

    def Prod14TupleIso[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Iso[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14])] =
      Iso((_: Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]).run)(Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](_: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14])))

    implicit def Prod14Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14])]): Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      new Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] {
        lazy val iso = Prod14TupleIso[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]
        def empty: Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = iso.reverseGet(M.empty)
        def combine(p1: Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], p2: Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
          iso.reverseGet(M.combine(iso.get(p1), iso.get(p2)))
      }

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A1]] = {
      val t = M.empty
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((x, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13, t.t14)))
    }

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A2]] = {
      val t = M.empty
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t.t1, x, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13, t.t14)))
    }

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A3]] = {
      val t = M.empty
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t.t1, t.t2, x, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13, t.t14)))
    }

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A4]] = {
      val t = M.empty
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t.t1, t.t2, t.t3, x, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13, t.t14)))
    }

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A5]] = {
      val t = M.empty
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t.t1, t.t2, t.t3, t.t4, x, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13, t.t14)))
    }

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A6]] = {
      val t = M.empty
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t.t1, t.t2, t.t3, t.t4, t.t5, x, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13, t.t14)))
    }

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A7]] = {
      val t = M.empty
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, x, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13, t.t14)))
    }

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A8]] = {
      val t = M.empty
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, x, t.t9, t.t10, t.t11, t.t12, t.t13, t.t14)))
    }

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A9]] = {
      val t = M.empty
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, x, t.t10, t.t11, t.t12, t.t13, t.t14)))
    }

    implicit def lifta9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A10]] = {
      val t = M.empty
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, x, t.t11, t.t12, t.t13, t.t14)))
    }

    implicit def lifta10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A11]] = {
      val t = M.empty
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, x, t.t12, t.t13, t.t14)))
    }

    implicit def lifta11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A12]] = {
      val t = M.empty
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, x, t.t13, t.t14)))
    }

    implicit def lifta12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A13]] = {
      val t = M.empty
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, x, t.t14)))
    }

    implicit def lifta13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A14]] = {
      val t = M.empty
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13, x)))
    }

    implicit def Prod14Lens0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A1]] =
      Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A1]](p => p.t1)(x => p =>
        Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((x, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14)))

    implicit def Prod14Lens1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A2]] =
      Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A2]](p => p.t2)(x => p =>
        Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, x, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14)))

    implicit def Prod14Lens2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A3]] =
      Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A3]](p => p.t3)(x => p =>
        Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, x, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14)))

    implicit def Prod14Lens3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A4]] =
      Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A4]](p => p.t4)(x => p =>
        Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, x, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14)))

    implicit def Prod14Lens4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A5]] =
      Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A5]](p => p.t5)(x => p =>
        Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, p.t4, x, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14)))

    implicit def Prod14Lens5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A6]] =
      Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A6]](p => p.t6)(x => p =>
        Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, p.t4, p.t5, x, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14)))

    implicit def Prod14Lens6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A7]] =
      Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A7]](p => p.t7)(x => p =>
        Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, x, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14)))

    implicit def Prod14Lens7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A8]] =
      Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A8]](p => p.t8)(x => p =>
        Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, x, p.t9, p.t10, p.t11, p.t12, p.t13, p.t14)))

    implicit def Prod14Lens8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A9]] =
      Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A9]](p => p.t9)(x => p =>
        Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, x, p.t10, p.t11, p.t12, p.t13, p.t14)))

    implicit def Prod14Lens9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A10]] =
      Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A10]](p => p.t10)(x => p =>
        Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, x, p.t11, p.t12, p.t13, p.t14)))

    implicit def Prod14Lens10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A11]] =
      Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A11]](p => p.t11)(x => p =>
        Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, x, p.t12, p.t13, p.t14)))

    implicit def Prod14Lens11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A12]] =
      Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A12]](p => p.t12)(x => p =>
        Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, x, p.t13, p.t14)))

    implicit def Prod14Lens12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A13]] =
      Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A13]](p => p.t13)(x => p =>
        Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, x, p.t14)))

    implicit def Prod14Lens13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A14]] =
      Lens[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A14]](p => p.t14)(x => p =>
        Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13, x)))

  }

  object Prod14 extends Prod14LP {

    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A1] =
      lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A2] =
      lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A3] =
      lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A4] =
      lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A5] =
      lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A6] =
      lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A7] =
      lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A8] =
      lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A9] =
      lifta8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A10] =
      lifta9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A11] =
      lifta10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A12] =
      lifta11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A13] =
      lifta12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A14] =
      lifta13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Prod14Lens0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A1] =
      Prod14Lens0[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Prod14Lens1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A2] =
      Prod14Lens1[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Prod14Lens2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A3] =
      Prod14Lens2[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Prod14Lens3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A4] =
      Prod14Lens3[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Prod14Lens4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A5] =
      Prod14Lens4[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Prod14Lens5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A6] =
      Prod14Lens5[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Prod14Lens6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A7] =
      Prod14Lens6[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Prod14Lens7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A8] =
      Prod14Lens7[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Prod14Lens8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A9] =
      Prod14Lens8[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Prod14Lens9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A10] =
      Prod14Lens9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Prod14Lens10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A11] =
      Prod14Lens10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Prod14Lens11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A12] =
      Prod14Lens11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Prod14Lens12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A13] =
      Prod14Lens12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Prod14Lens13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Lens[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A14] =
      Prod14Lens13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

  }

  @newtype case class Cop14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](run: Either[F[A1], Either[F[A2], Either[F[A3], Either[F[A4], Either[F[A5], Either[F[A6], Either[F[A7], Either[F[A8], Either[F[A9], Either[F[A10], Either[F[A11], Either[F[A12], Either[F[A13], F[A14]]]]]]]]]]]]]]) {
    private def mapN = new Map14C[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14]] {}

    def map1[B](f: F[A1] => F[B]): Cop14[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
      Cop14[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Cop14[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
      Cop14[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Cop14[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
      Cop14[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Cop14[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
      Cop14[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](mapN.mapAt(f)(run))

    def map3[B](f: F[A3] => F[B]): Cop14[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
      Cop14[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](mapN.map3(run)(f))

    def mapAt[B](f: F[A3] => F[B])(implicit d: Dummy3): Cop14[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
      Cop14[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](mapN.mapAt(f)(run))

    def map4[B](f: F[A4] => F[B]): Cop14[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
      Cop14[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](mapN.map4(run)(f))

    def mapAt[B](f: F[A4] => F[B])(implicit d: Dummy4): Cop14[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
      Cop14[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](mapN.mapAt(f)(run))

    def map5[B](f: F[A5] => F[B]): Cop14[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
      Cop14[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14](mapN.map5(run)(f))

    def mapAt[B](f: F[A5] => F[B])(implicit d: Dummy5): Cop14[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
      Cop14[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14](mapN.mapAt(f)(run))

    def map6[B](f: F[A6] => F[B]): Cop14[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14] =
      Cop14[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14](mapN.map6(run)(f))

    def mapAt[B](f: F[A6] => F[B])(implicit d: Dummy6): Cop14[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14] =
      Cop14[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14](mapN.mapAt(f)(run))

    def map7[B](f: F[A7] => F[B]): Cop14[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14] =
      Cop14[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14](mapN.map7(run)(f))

    def mapAt[B](f: F[A7] => F[B])(implicit d: Dummy7): Cop14[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14] =
      Cop14[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14](mapN.mapAt(f)(run))

    def map8[B](f: F[A8] => F[B]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14] =
      Cop14[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14](mapN.map8(run)(f))

    def mapAt[B](f: F[A8] => F[B])(implicit d: Dummy8): Cop14[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14] =
      Cop14[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14](mapN.mapAt(f)(run))

    def map9[B](f: F[A9] => F[B]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14] =
      Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14](mapN.map9(run)(f))

    def mapAt[B](f: F[A9] => F[B])(implicit d: Dummy9): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14] =
      Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14](mapN.mapAt(f)(run))

    def map10[B](f: F[A10] => F[B]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14] =
      Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14](mapN.map10(run)(f))

    def mapAt[B](f: F[A10] => F[B])(implicit d: Dummy10): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14] =
      Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14](mapN.mapAt(f)(run))

    def map11[B](f: F[A11] => F[B]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14] =
      Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14](mapN.map11(run)(f))

    def mapAt[B](f: F[A11] => F[B])(implicit d: Dummy11): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14] =
      Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14](mapN.mapAt(f)(run))

    def map12[B](f: F[A12] => F[B]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14] =
      Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14](mapN.map12(run)(f))

    def mapAt[B](f: F[A12] => F[B])(implicit d: Dummy12): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14] =
      Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14](mapN.mapAt(f)(run))

    def map13[B](f: F[A13] => F[B]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14] =
      Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14](mapN.map13(run)(f))

    def mapAt[B](f: F[A13] => F[B])(implicit d: Dummy13): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14] =
      Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14](mapN.mapAt(f)(run))

    def map14[B](f: F[A14] => F[B]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B] =
      Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B](mapN.map14(run)(f))

    def mapAt[B](f: F[A14] => F[B])(implicit d: Dummy14): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B] =
      Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B](mapN.mapAt(f)(run))

  }

  trait Cop14LP {

    implicit def Cop14Instance[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: FFunctor[Cop14[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] with FTraverseCop[Cop14[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      new FFunctor[Cop14[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] with FTraverseCop[Cop14[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] {
        def map[F[_], G[_]](c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14])(nt: F ~> G): Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
          Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](c.run.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_)))))))))))))))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14])(f: F ~> Lambda[a => A[G[a]]]): A[Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
          c.run match {

            case Left(x) => Functor[A].map(f(x))(y => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Left(y)))

            case Right(Left(x)) => Functor[A].map(f(x))(y => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Left(y))))

            case Right(Right(Left(x))) => Functor[A].map(f(x))(y => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Left(y)))))

            case Right(Right(Right(Left(x)))) => Functor[A].map(f(x))(y => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Left(y))))))

            case Right(Right(Right(Right(Left(x))))) => Functor[A].map(f(x))(y => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Left(y)))))))

            case Right(Right(Right(Right(Right(Left(x)))))) => Functor[A].map(f(x))(y => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Left(y))))))))

            case Right(Right(Right(Right(Right(Right(Left(x))))))) => Functor[A].map(f(x))(y => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Left(y)))))))))

            case Right(Right(Right(Right(Right(Right(Right(Left(x)))))))) => Functor[A].map(f(x))(y => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Left(y))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))) => Functor[A].map(f(x))(y => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Left(y)))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))) => Functor[A].map(f(x))(y => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(y))))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))) => Functor[A].map(f(x))(y => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(y)))))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))))) => Functor[A].map(f(x))(y => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(y))))))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))))) => Functor[A].map(f(x))(y => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(y)))))))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(x))))))))))))) => Functor[A].map(f(x))(y => Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(y)))))))))))))))

          }
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A1]] =
      Inj.instance(x => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Left(x)))

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A2]] =
      Inj.instance(x => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Left(x))))

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A3]] =
      Inj.instance(x => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Left(x)))))

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A4]] =
      Inj.instance(x => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Left(x))))))

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A5]] =
      Inj.instance(x => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Left(x)))))))

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A6]] =
      Inj.instance(x => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Left(x))))))))

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A7]] =
      Inj.instance(x => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Left(x)))))))))

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A8]] =
      Inj.instance(x => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A9]] =
      Inj.instance(x => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))))

    implicit def inja9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A10]] =
      Inj.instance(x => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))))

    implicit def inja10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A11]] =
      Inj.instance(x => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))))))

    implicit def inja11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A12]] =
      Inj.instance(x => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))))))

    implicit def inja12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A13]] =
      Inj.instance(x => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))))))))

    implicit def inja13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A14]] =
      Inj.instance(x => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(x)))))))))))))))

    implicit def Cop14Optional0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A1]] =
      Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A1]](c => c.run match {
        case Left(x) => Some(x)
        case _ => None
      })(x => _ => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Left(x)))

    implicit def Cop14Optional1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A2]] =
      Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A2]](c => c.run match {
        case Right(Left(x)) => Some(x)
        case _ => None
      })(x => _ => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Left(x))))

    implicit def Cop14Optional2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A3]] =
      Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A3]](c => c.run match {
        case Right(Right(Left(x))) => Some(x)
        case _ => None
      })(x => _ => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Left(x)))))

    implicit def Cop14Optional3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A4]] =
      Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A4]](c => c.run match {
        case Right(Right(Right(Left(x)))) => Some(x)
        case _ => None
      })(x => _ => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Left(x))))))

    implicit def Cop14Optional4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A5]] =
      Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A5]](c => c.run match {
        case Right(Right(Right(Right(Left(x))))) => Some(x)
        case _ => None
      })(x => _ => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Left(x)))))))

    implicit def Cop14Optional5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A6]] =
      Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A6]](c => c.run match {
        case Right(Right(Right(Right(Right(Left(x)))))) => Some(x)
        case _ => None
      })(x => _ => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Left(x))))))))

    implicit def Cop14Optional6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A7]] =
      Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A7]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Left(x))))))) => Some(x)
        case _ => None
      })(x => _ => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Left(x)))))))))

    implicit def Cop14Optional7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A8]] =
      Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A8]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Right(Left(x)))))))) => Some(x)
        case _ => None
      })(x => _ => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))

    implicit def Cop14Optional8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A9]] =
      Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A9]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))) => Some(x)
        case _ => None
      })(x => _ => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))))

    implicit def Cop14Optional9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A10]] =
      Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A10]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))) => Some(x)
        case _ => None
      })(x => _ => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))))

    implicit def Cop14Optional10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A11]] =
      Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A11]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))) => Some(x)
        case _ => None
      })(x => _ => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))))))

    implicit def Cop14Optional11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A12]] =
      Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A12]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))))) => Some(x)
        case _ => None
      })(x => _ => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))))))

    implicit def Cop14Optional12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A13]] =
      Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A13]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))))) => Some(x)
        case _ => None
      })(x => _ => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))))))))

    implicit def Cop14Optional13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A14]] =
      Optional[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A14]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(x))))))))))))) => Some(x)
        case _ => None
      })(x => _ => Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(x)))))))))))))))

  }

  object Cop14 extends Cop14LP {

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A1] =
      inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A2] =
      inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A3] =
      inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A4] =
      inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A5] =
      inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A6] =
      inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A7] =
      inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A8] =
      inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A9] =
      inja8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A10] =
      inja9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A11] =
      inja10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A12] =
      inja11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A13] =
      inja12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A14] =
      inja13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Cop14Optional0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A1] =
      Cop14Optional0[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Cop14Optional1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A2] =
      Cop14Optional1[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Cop14Optional2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A3] =
      Cop14Optional2[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Cop14Optional3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A4] =
      Cop14Optional3[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Cop14Optional4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A5] =
      Cop14Optional4[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Cop14Optional5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A6] =
      Cop14Optional5[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Cop14Optional6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A7] =
      Cop14Optional6[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Cop14Optional7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A8] =
      Cop14Optional7[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Cop14Optional8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A9] =
      Cop14Optional8[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Cop14Optional9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A10] =
      Cop14Optional9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Cop14Optional10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A11] =
      Cop14Optional10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Cop14Optional11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A12] =
      Cop14Optional11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Cop14Optional12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A13] =
      Cop14Optional12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def Cop14Optional13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Optional[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A14] =
      Cop14Optional13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

  }
}
