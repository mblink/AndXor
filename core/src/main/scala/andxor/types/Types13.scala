package andxor.types

import andxor._
import scalaz.{\/, -\/, \/-, ~>, Applicative, Functor, Lens, Monoid, PLens, PlusEmpty, StoreT}
import scalaz.Id.Id
import scalaz.Isomorphism.IsoSet

trait Types13 {
  @newtype case class Prod13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13])) { self =>
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

    private def mapN = new Map13P[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13]] {}

    def map1[B](f: F[A1] => F[B]): Prod13[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
      Prod13[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Prod13[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
      Prod13[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Prod13[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
      Prod13[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Prod13[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
      Prod13[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](mapN.mapAt(f)(run))

    def map3[B](f: F[A3] => F[B]): Prod13[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
      Prod13[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](mapN.map3(run)(f))

    def mapAt[B](f: F[A3] => F[B])(implicit d: Dummy3): Prod13[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
      Prod13[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](mapN.mapAt(f)(run))

    def map4[B](f: F[A4] => F[B]): Prod13[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
      Prod13[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13](mapN.map4(run)(f))

    def mapAt[B](f: F[A4] => F[B])(implicit d: Dummy4): Prod13[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
      Prod13[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13](mapN.mapAt(f)(run))

    def map5[B](f: F[A5] => F[B]): Prod13[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13] =
      Prod13[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13](mapN.map5(run)(f))

    def mapAt[B](f: F[A5] => F[B])(implicit d: Dummy5): Prod13[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13] =
      Prod13[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13](mapN.mapAt(f)(run))

    def map6[B](f: F[A6] => F[B]): Prod13[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13] =
      Prod13[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13](mapN.map6(run)(f))

    def mapAt[B](f: F[A6] => F[B])(implicit d: Dummy6): Prod13[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13] =
      Prod13[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13](mapN.mapAt(f)(run))

    def map7[B](f: F[A7] => F[B]): Prod13[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13] =
      Prod13[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13](mapN.map7(run)(f))

    def mapAt[B](f: F[A7] => F[B])(implicit d: Dummy7): Prod13[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13] =
      Prod13[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13](mapN.mapAt(f)(run))

    def map8[B](f: F[A8] => F[B]): Prod13[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13] =
      Prod13[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13](mapN.map8(run)(f))

    def mapAt[B](f: F[A8] => F[B])(implicit d: Dummy8): Prod13[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13] =
      Prod13[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13](mapN.mapAt(f)(run))

    def map9[B](f: F[A9] => F[B]): Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13] =
      Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13](mapN.map9(run)(f))

    def mapAt[B](f: F[A9] => F[B])(implicit d: Dummy9): Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13] =
      Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13](mapN.mapAt(f)(run))

    def map10[B](f: F[A10] => F[B]): Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13] =
      Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13](mapN.map10(run)(f))

    def mapAt[B](f: F[A10] => F[B])(implicit d: Dummy10): Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13] =
      Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13](mapN.mapAt(f)(run))

    def map11[B](f: F[A11] => F[B]): Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13] =
      Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13](mapN.map11(run)(f))

    def mapAt[B](f: F[A11] => F[B])(implicit d: Dummy11): Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13] =
      Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13](mapN.mapAt(f)(run))

    def map12[B](f: F[A12] => F[B]): Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13] =
      Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13](mapN.map12(run)(f))

    def mapAt[B](f: F[A12] => F[B])(implicit d: Dummy12): Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13] =
      Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13](mapN.mapAt(f)(run))

    def map13[B](f: F[A13] => F[B]): Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B] =
      Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B](mapN.map13(run)(f))

    def mapAt[B](f: F[A13] => F[B])(implicit d: Dummy13): Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B] =
      Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B](mapN.mapAt(f)(run))

  }

  trait Prod13LP {

    implicit def Prod13Instance[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: FFunctor[Prod13[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] with FTraverseProd[Prod13[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      new FFunctor[Prod13[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] with FTraverseProd[Prod13[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] {
        def map[F[_], G[_]](p: Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13])(nt: F ~> G): Prod13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
          Prod13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((nt(p.t1), nt(p.t2), nt(p.t3), nt(p.t4), nt(p.t5), nt(p.t6), nt(p.t7), nt(p.t8), nt(p.t9), nt(p.t10), nt(p.t11), nt(p.t12), nt(p.t13)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13])(f: F ~> Lambda[a => A[G[a]]]): A[Prod13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
          Applicative[A].ap(f(p.t13))(Applicative[A].ap(f(p.t12))(Applicative[A].ap(f(p.t11))(Applicative[A].ap(f(p.t10))(Applicative[A].ap(f(p.t9))(Applicative[A].ap(f(p.t8))(Applicative[A].ap(f(p.t7))(Applicative[A].ap(f(p.t6))(Applicative[A].ap(f(p.t5))(Applicative[A].ap(f(p.t4))(Applicative[A].ap(f(p.t3))(Applicative[A].ap(f(p.t2))(Applicative[A].map(f(p.t1))((i0: G[A1]) => (i1: G[A2]) => (i2: G[A3]) => (i3: G[A4]) => (i4: G[A5]) => (i5: G[A6]) => (i6: G[A7]) => (i7: G[A8]) => (i8: G[A9]) => (i9: G[A10]) => (i10: G[A11]) => (i11: G[A12]) => (i12: G[A13]) => Prod13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12)))))))))))))))
      }

    implicit def Prod13FoldMap[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: FoldMap[Prod13[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], Cop13[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      new FoldMap[Prod13[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], Cop13[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] {
        def emptyProd[F[_]](implicit PE: PlusEmpty[F]): Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
          Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5], PE.empty[A6], PE.empty[A7], PE.empty[A8], PE.empty[A9], PE.empty[A10], PE.empty[A11], PE.empty[A12], PE.empty[A13]))

        def unconsAll[F[_], G[_]](p: Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13])(implicit U: Uncons[F, G]): (List[Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]) = {
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
          (
            List(h1.map(Inj[Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], G[A1]].apply(_)), h2.map(Inj[Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], G[A2]].apply(_)), h3.map(Inj[Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], G[A3]].apply(_)), h4.map(Inj[Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], G[A4]].apply(_)), h5.map(Inj[Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], G[A5]].apply(_)), h6.map(Inj[Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], G[A6]].apply(_)), h7.map(Inj[Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], G[A7]].apply(_)), h8.map(Inj[Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], G[A8]].apply(_)), h9.map(Inj[Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], G[A9]].apply(_)), h10.map(Inj[Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], G[A10]].apply(_)), h11.map(Inj[Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], G[A11]].apply(_)), h12.map(Inj[Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], G[A12]].apply(_)), h13.map(Inj[Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], G[A13]].apply(_))).flatten,
            Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)))
        }

        def unconsOne[F[_], G[_]](p: Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], c: Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13])(implicit U: Uncons[F, G]): (Option[Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]) =
          c.run match {

            case -\/(_) =>
              val (h, t) = U(p.t1)
              (h.map(v => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](-\/(v))), Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13)))

            case \/-(-\/(_)) =>
              val (h, t) = U(p.t2)
              (h.map(v => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(-\/(v)))), Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, t, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13)))

            case \/-(\/-(-\/(_))) =>
              val (h, t) = U(p.t3)
              (h.map(v => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(-\/(v))))), Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, t, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13)))

            case \/-(\/-(\/-(-\/(_)))) =>
              val (h, t) = U(p.t4)
              (h.map(v => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(-\/(v)))))), Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, p.t3, t, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13)))

            case \/-(\/-(\/-(\/-(-\/(_))))) =>
              val (h, t) = U(p.t5)
              (h.map(v => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(-\/(v))))))), Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, p.t3, p.t4, t, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13)))

            case \/-(\/-(\/-(\/-(\/-(-\/(_)))))) =>
              val (h, t) = U(p.t6)
              (h.map(v => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(-\/(v)))))))), Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, p.t3, p.t4, p.t5, t, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13)))

            case \/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))) =>
              val (h, t) = U(p.t7)
              (h.map(v => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(-\/(v))))))))), Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, t, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))) =>
              val (h, t) = U(p.t8)
              (h.map(v => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v)))))))))), Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, t, p.t9, p.t10, p.t11, p.t12, p.t13)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))) =>
              val (h, t) = U(p.t9)
              (h.map(v => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v))))))))))), Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, t, p.t10, p.t11, p.t12, p.t13)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))))) =>
              val (h, t) = U(p.t10)
              (h.map(v => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v)))))))))))), Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, t, p.t11, p.t12, p.t13)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))))) =>
              val (h, t) = U(p.t11)
              (h.map(v => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v))))))))))))), Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, t, p.t12, p.t13)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))))))) =>
              val (h, t) = U(p.t12)
              (h.map(v => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v)))))))))))))), Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, t, p.t13)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(_)))))))))))) =>
              val (h, t) = U(p.t13)
              (h.map(v => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(v)))))))))))))), Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, t)))

          }
      }

    def Prod13TupleIso[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: IsoSet[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13])] =
      IsoSet((_: Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]).run, Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](_: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13])))

    implicit def Prod13Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13])]): Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      Monoid.fromIso(Prod13TupleIso[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13])(M)

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A1]] = {
      val t = M.zero
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((x, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13)))
    }

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A2]] = {
      val t = M.zero
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t.t1, x, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13)))
    }

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A3]] = {
      val t = M.zero
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t.t1, t.t2, x, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13)))
    }

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A4]] = {
      val t = M.zero
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t.t1, t.t2, t.t3, x, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13)))
    }

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A5]] = {
      val t = M.zero
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t.t1, t.t2, t.t3, t.t4, x, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13)))
    }

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A6]] = {
      val t = M.zero
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t.t1, t.t2, t.t3, t.t4, t.t5, x, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13)))
    }

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A7]] = {
      val t = M.zero
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, x, t.t8, t.t9, t.t10, t.t11, t.t12, t.t13)))
    }

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A8]] = {
      val t = M.zero
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, x, t.t9, t.t10, t.t11, t.t12, t.t13)))
    }

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A9]] = {
      val t = M.zero
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, x, t.t10, t.t11, t.t12, t.t13)))
    }

    implicit def lifta9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A10]] = {
      val t = M.zero
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, x, t.t11, t.t12, t.t13)))
    }

    implicit def lifta10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A11]] = {
      val t = M.zero
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, x, t.t12, t.t13)))
    }

    implicit def lifta11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A12]] = {
      val t = M.zero
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, x, t.t13)))
    }

    implicit def lifta12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A13]] = {
      val t = M.zero
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12, x)))
    }

    implicit def Prod13Lens0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A1]] =
      Lens(p => StoreT.store[F[A1], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](p.t1)(x =>
        Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((x, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13))))

    implicit def Prod13Lens1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A2]] =
      Lens(p => StoreT.store[F[A2], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](p.t2)(x =>
        Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, x, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13))))

    implicit def Prod13Lens2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A3]] =
      Lens(p => StoreT.store[F[A3], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](p.t3)(x =>
        Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, x, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13))))

    implicit def Prod13Lens3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A4]] =
      Lens(p => StoreT.store[F[A4], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](p.t4)(x =>
        Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, p.t3, x, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13))))

    implicit def Prod13Lens4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A5]] =
      Lens(p => StoreT.store[F[A5], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](p.t5)(x =>
        Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, p.t3, p.t4, x, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13))))

    implicit def Prod13Lens5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A6]] =
      Lens(p => StoreT.store[F[A6], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](p.t6)(x =>
        Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, p.t3, p.t4, p.t5, x, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13))))

    implicit def Prod13Lens6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A7]] =
      Lens(p => StoreT.store[F[A7], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](p.t7)(x =>
        Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, x, p.t8, p.t9, p.t10, p.t11, p.t12, p.t13))))

    implicit def Prod13Lens7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A8]] =
      Lens(p => StoreT.store[F[A8], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](p.t8)(x =>
        Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, x, p.t9, p.t10, p.t11, p.t12, p.t13))))

    implicit def Prod13Lens8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A9]] =
      Lens(p => StoreT.store[F[A9], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](p.t9)(x =>
        Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, x, p.t10, p.t11, p.t12, p.t13))))

    implicit def Prod13Lens9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A10]] =
      Lens(p => StoreT.store[F[A10], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](p.t10)(x =>
        Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, x, p.t11, p.t12, p.t13))))

    implicit def Prod13Lens10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A11]] =
      Lens(p => StoreT.store[F[A11], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](p.t11)(x =>
        Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, x, p.t12, p.t13))))

    implicit def Prod13Lens11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A12]] =
      Lens(p => StoreT.store[F[A12], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](p.t12)(x =>
        Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, x, p.t13))))

    implicit def Prod13Lens12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A13]] =
      Lens(p => StoreT.store[F[A13], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](p.t13)(x =>
        Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12, x))))

  }

  object Prod13 extends Prod13LP {

    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A1] =
      lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A2] =
      lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A3] =
      lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A4] =
      lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A5] =
      lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A6] =
      lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A7] =
      lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A8] =
      lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A9] =
      lifta8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A10] =
      lifta9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A11] =
      lifta10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A12] =
      lifta11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A13] =
      lifta12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Prod13Lens0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A1] =
      Prod13Lens0[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Prod13Lens1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A2] =
      Prod13Lens1[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Prod13Lens2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A3] =
      Prod13Lens2[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Prod13Lens3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A4] =
      Prod13Lens3[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Prod13Lens4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A5] =
      Prod13Lens4[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Prod13Lens5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A6] =
      Prod13Lens5[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Prod13Lens6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A7] =
      Prod13Lens6[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Prod13Lens7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A8] =
      Prod13Lens7[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Prod13Lens8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A9] =
      Prod13Lens8[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Prod13Lens9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A10] =
      Prod13Lens9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Prod13Lens10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A11] =
      Prod13Lens10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Prod13Lens11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A12] =
      Prod13Lens11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Prod13Lens12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Lens[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A13] =
      Prod13Lens12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

  }

  @newtype case class Cop13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ F[A13]))))))))))))) {
    private def mapN = new Map13C[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13]] {}

    def map1[B](f: F[A1] => F[B]): Cop13[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
      Cop13[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Cop13[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
      Cop13[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Cop13[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
      Cop13[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Cop13[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
      Cop13[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](mapN.mapAt(f)(run))

    def map3[B](f: F[A3] => F[B]): Cop13[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
      Cop13[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](mapN.map3(run)(f))

    def mapAt[B](f: F[A3] => F[B])(implicit d: Dummy3): Cop13[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
      Cop13[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](mapN.mapAt(f)(run))

    def map4[B](f: F[A4] => F[B]): Cop13[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
      Cop13[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13](mapN.map4(run)(f))

    def mapAt[B](f: F[A4] => F[B])(implicit d: Dummy4): Cop13[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
      Cop13[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13](mapN.mapAt(f)(run))

    def map5[B](f: F[A5] => F[B]): Cop13[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13] =
      Cop13[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13](mapN.map5(run)(f))

    def mapAt[B](f: F[A5] => F[B])(implicit d: Dummy5): Cop13[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13] =
      Cop13[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13](mapN.mapAt(f)(run))

    def map6[B](f: F[A6] => F[B]): Cop13[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13] =
      Cop13[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13](mapN.map6(run)(f))

    def mapAt[B](f: F[A6] => F[B])(implicit d: Dummy6): Cop13[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13] =
      Cop13[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13](mapN.mapAt(f)(run))

    def map7[B](f: F[A7] => F[B]): Cop13[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13] =
      Cop13[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13](mapN.map7(run)(f))

    def mapAt[B](f: F[A7] => F[B])(implicit d: Dummy7): Cop13[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13] =
      Cop13[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13](mapN.mapAt(f)(run))

    def map8[B](f: F[A8] => F[B]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13] =
      Cop13[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13](mapN.map8(run)(f))

    def mapAt[B](f: F[A8] => F[B])(implicit d: Dummy8): Cop13[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13] =
      Cop13[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13](mapN.mapAt(f)(run))

    def map9[B](f: F[A9] => F[B]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13] =
      Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13](mapN.map9(run)(f))

    def mapAt[B](f: F[A9] => F[B])(implicit d: Dummy9): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13] =
      Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13](mapN.mapAt(f)(run))

    def map10[B](f: F[A10] => F[B]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13] =
      Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13](mapN.map10(run)(f))

    def mapAt[B](f: F[A10] => F[B])(implicit d: Dummy10): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13] =
      Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13](mapN.mapAt(f)(run))

    def map11[B](f: F[A11] => F[B]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13] =
      Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13](mapN.map11(run)(f))

    def mapAt[B](f: F[A11] => F[B])(implicit d: Dummy11): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13] =
      Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13](mapN.mapAt(f)(run))

    def map12[B](f: F[A12] => F[B]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13] =
      Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13](mapN.map12(run)(f))

    def mapAt[B](f: F[A12] => F[B])(implicit d: Dummy12): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13] =
      Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13](mapN.mapAt(f)(run))

    def map13[B](f: F[A13] => F[B]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B] =
      Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B](mapN.map13(run)(f))

    def mapAt[B](f: F[A13] => F[B])(implicit d: Dummy13): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B] =
      Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B](mapN.mapAt(f)(run))

  }

  trait Cop13LP {

    implicit def Cop13Instance[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: FFunctor[Cop13[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] with FTraverseCop[Cop13[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      new FFunctor[Cop13[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] with FTraverseCop[Cop13[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] {
        def map[F[_], G[_]](c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13])(nt: F ~> G): Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
          Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](c.run.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_))))))))))))))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13])(f: F ~> Lambda[a => A[G[a]]]): A[Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
          c.run match {

            case -\/(x) => Functor[A].map(f(x))(y => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](-\/(y)))

            case \/-(-\/(x)) => Functor[A].map(f(x))(y => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(-\/(y))))

            case \/-(\/-(-\/(x))) => Functor[A].map(f(x))(y => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(-\/(y)))))

            case \/-(\/-(\/-(-\/(x)))) => Functor[A].map(f(x))(y => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(-\/(y))))))

            case \/-(\/-(\/-(\/-(-\/(x))))) => Functor[A].map(f(x))(y => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(-\/(y)))))))

            case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Functor[A].map(f(x))(y => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(-\/(y))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Functor[A].map(f(x))(y => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Functor[A].map(f(x))(y => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Functor[A].map(f(x))(y => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Functor[A].map(f(x))(y => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Functor[A].map(f(x))(y => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Functor[A].map(f(x))(y => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))) => Functor[A].map(f(x))(y => Cop13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(y))))))))))))))

          }
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A1]] =
      Inj.instance(x => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](-\/(x)))

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A2]] =
      Inj.instance(x => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(-\/(x))))

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A3]] =
      Inj.instance(x => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(-\/(x)))))

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A4]] =
      Inj.instance(x => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(-\/(x))))))

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A5]] =
      Inj.instance(x => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(-\/(x)))))))

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A6]] =
      Inj.instance(x => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(-\/(x))))))))

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A7]] =
      Inj.instance(x => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A8]] =
      Inj.instance(x => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A9]] =
      Inj.instance(x => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))

    implicit def inja9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A10]] =
      Inj.instance(x => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))

    implicit def inja10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A11]] =
      Inj.instance(x => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))

    implicit def inja11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A12]] =
      Inj.instance(x => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))

    implicit def inja12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A13]] =
      Inj.instance(x => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))

    implicit def Cop13PLens0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A1]] =
      PLens(c => c.run match {
        case -\/(x) => Some(StoreT.store[F[A1], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](x)(y => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](-\/(y))))
        case _ => None
      })

    implicit def Cop13PLens1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A2]] =
      PLens(c => c.run match {
        case \/-(-\/(x)) => Some(StoreT.store[F[A2], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](x)(y => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(-\/(y)))))
        case _ => None
      })

    implicit def Cop13PLens2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A3]] =
      PLens(c => c.run match {
        case \/-(\/-(-\/(x))) => Some(StoreT.store[F[A3], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](x)(y => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(-\/(y))))))
        case _ => None
      })

    implicit def Cop13PLens3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A4]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(StoreT.store[F[A4], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](x)(y => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(-\/(y)))))))
        case _ => None
      })

    implicit def Cop13PLens4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A5]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(StoreT.store[F[A5], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](x)(y => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(-\/(y))))))))
        case _ => None
      })

    implicit def Cop13PLens5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A6]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(StoreT.store[F[A6], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](x)(y => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))
        case _ => None
      })

    implicit def Cop13PLens6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A7]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(StoreT.store[F[A7], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](x)(y => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))
        case _ => None
      })

    implicit def Cop13PLens7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A8]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(StoreT.store[F[A8], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](x)(y => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))
        case _ => None
      })

    implicit def Cop13PLens8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A9]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(StoreT.store[F[A9], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](x)(y => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))
        case _ => None
      })

    implicit def Cop13PLens9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A10]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(StoreT.store[F[A10], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](x)(y => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))
        case _ => None
      })

    implicit def Cop13PLens10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A11]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(StoreT.store[F[A11], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](x)(y => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))
        case _ => None
      })

    implicit def Cop13PLens11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A12]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(StoreT.store[F[A12], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](x)(y => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))
        case _ => None
      })

    implicit def Cop13PLens12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A13]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))) => Some(StoreT.store[F[A13], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](x)(y => Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(y)))))))))))))))
        case _ => None
      })

  }

  object Cop13 extends Cop13LP {

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A1] =
      inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A2] =
      inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A3] =
      inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A4] =
      inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A5] =
      inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A6] =
      inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A7] =
      inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A8] =
      inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A9] =
      inja8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A10] =
      inja9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A11] =
      inja10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A12] =
      inja11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A13] =
      inja12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Cop13PLens0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A1] =
      Cop13PLens0[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Cop13PLens1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A2] =
      Cop13PLens1[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Cop13PLens2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A3] =
      Cop13PLens2[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Cop13PLens3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A4] =
      Cop13PLens3[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Cop13PLens4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A5] =
      Cop13PLens4[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Cop13PLens5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A6] =
      Cop13PLens5[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Cop13PLens6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A7] =
      Cop13PLens6[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Cop13PLens7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A8] =
      Cop13PLens7[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Cop13PLens8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A9] =
      Cop13PLens8[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Cop13PLens9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A10] =
      Cop13PLens9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Cop13PLens10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A11] =
      Cop13PLens10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Cop13PLens11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A12] =
      Cop13PLens11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def Cop13PLens12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: PLens[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A13] =
      Cop13PLens12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

  }
}
