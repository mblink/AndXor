package andxor.types

import andxor._
import scalaz.{\/, -\/, \/-, ~>, Applicative, Functor, Lens, Monoid, PLens, PlusEmpty, StoreT}
import scalaz.Id.Id
import scalaz.Isomorphism.IsoSet

trait Types11 {
  @newtype case class Prod11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11])) { self =>
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

    private def mapN = new Map11P[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11]] {}

    def map1[B](f: F[A1] => F[B]): Prod11[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] =
      Prod11[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Prod11[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] =
      Prod11[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Prod11[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11] =
      Prod11[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Prod11[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11] =
      Prod11[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11](mapN.mapAt(f)(run))

    def map3[B](f: F[A3] => F[B]): Prod11[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11] =
      Prod11[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11](mapN.map3(run)(f))

    def mapAt[B](f: F[A3] => F[B])(implicit d: Dummy3): Prod11[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11] =
      Prod11[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11](mapN.mapAt(f)(run))

    def map4[B](f: F[A4] => F[B]): Prod11[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11] =
      Prod11[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11](mapN.map4(run)(f))

    def mapAt[B](f: F[A4] => F[B])(implicit d: Dummy4): Prod11[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11] =
      Prod11[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11](mapN.mapAt(f)(run))

    def map5[B](f: F[A5] => F[B]): Prod11[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11] =
      Prod11[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11](mapN.map5(run)(f))

    def mapAt[B](f: F[A5] => F[B])(implicit d: Dummy5): Prod11[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11] =
      Prod11[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11](mapN.mapAt(f)(run))

    def map6[B](f: F[A6] => F[B]): Prod11[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11] =
      Prod11[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11](mapN.map6(run)(f))

    def mapAt[B](f: F[A6] => F[B])(implicit d: Dummy6): Prod11[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11] =
      Prod11[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11](mapN.mapAt(f)(run))

    def map7[B](f: F[A7] => F[B]): Prod11[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11] =
      Prod11[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11](mapN.map7(run)(f))

    def mapAt[B](f: F[A7] => F[B])(implicit d: Dummy7): Prod11[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11] =
      Prod11[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11](mapN.mapAt(f)(run))

    def map8[B](f: F[A8] => F[B]): Prod11[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11] =
      Prod11[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11](mapN.map8(run)(f))

    def mapAt[B](f: F[A8] => F[B])(implicit d: Dummy8): Prod11[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11] =
      Prod11[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11](mapN.mapAt(f)(run))

    def map9[B](f: F[A9] => F[B]): Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11] =
      Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11](mapN.map9(run)(f))

    def mapAt[B](f: F[A9] => F[B])(implicit d: Dummy9): Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11] =
      Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11](mapN.mapAt(f)(run))

    def map10[B](f: F[A10] => F[B]): Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11] =
      Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11](mapN.map10(run)(f))

    def mapAt[B](f: F[A10] => F[B])(implicit d: Dummy10): Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11] =
      Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11](mapN.mapAt(f)(run))

    def map11[B](f: F[A11] => F[B]): Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B] =
      Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B](mapN.map11(run)(f))

    def mapAt[B](f: F[A11] => F[B])(implicit d: Dummy11): Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B] =
      Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B](mapN.mapAt(f)(run))

  }

  trait Prod11LP {

    implicit def Prod11Instance[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: FFunctor[Prod11[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] with FTraverseProd[Prod11[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      new FFunctor[Prod11[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] with FTraverseProd[Prod11[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] {
        def map[F[_], G[_]](p: Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11])(nt: F ~> G): Prod11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] =
          Prod11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((nt(p.t1), nt(p.t2), nt(p.t3), nt(p.t4), nt(p.t5), nt(p.t6), nt(p.t7), nt(p.t8), nt(p.t9), nt(p.t10), nt(p.t11)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11])(f: F ~> Lambda[a => A[G[a]]]): A[Prod11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
          Applicative[A].ap(f(p.t11))(Applicative[A].ap(f(p.t10))(Applicative[A].ap(f(p.t9))(Applicative[A].ap(f(p.t8))(Applicative[A].ap(f(p.t7))(Applicative[A].ap(f(p.t6))(Applicative[A].ap(f(p.t5))(Applicative[A].ap(f(p.t4))(Applicative[A].ap(f(p.t3))(Applicative[A].ap(f(p.t2))(Applicative[A].map(f(p.t1))((i0: G[A1]) => (i1: G[A2]) => (i2: G[A3]) => (i3: G[A4]) => (i4: G[A5]) => (i5: G[A6]) => (i6: G[A7]) => (i7: G[A8]) => (i8: G[A9]) => (i9: G[A10]) => (i10: G[A11]) => Prod11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10)))))))))))))
      }

    implicit def Prod11FoldMap[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: FoldMap[Prod11[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], Cop11[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      new FoldMap[Prod11[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], Cop11[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] {
        def emptyProd[F[_]](implicit PE: PlusEmpty[F]): Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] =
          Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5], PE.empty[A6], PE.empty[A7], PE.empty[A8], PE.empty[A9], PE.empty[A10], PE.empty[A11]))

        def unconsAll[F[_], G[_]](p: Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11])(implicit U: Uncons[F, G]): (List[Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]) = {
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
          (
            List(h1.map(Inj[Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], G[A1]].apply(_)), h2.map(Inj[Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], G[A2]].apply(_)), h3.map(Inj[Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], G[A3]].apply(_)), h4.map(Inj[Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], G[A4]].apply(_)), h5.map(Inj[Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], G[A5]].apply(_)), h6.map(Inj[Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], G[A6]].apply(_)), h7.map(Inj[Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], G[A7]].apply(_)), h8.map(Inj[Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], G[A8]].apply(_)), h9.map(Inj[Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], G[A9]].apply(_)), h10.map(Inj[Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], G[A10]].apply(_)), h11.map(Inj[Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], G[A11]].apply(_))).flatten,
            Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)))
        }

        def unconsOne[F[_], G[_]](p: Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], c: Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11])(implicit U: Uncons[F, G]): (Option[Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]) =
          c.run match {

            case -\/(_) =>
              val (h, t) = U(p.t1)
              (h.map(v => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](-\/(v))), Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11)))

            case \/-(-\/(_)) =>
              val (h, t) = U(p.t2)
              (h.map(v => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(-\/(v)))), Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((p.t1, t, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11)))

            case \/-(\/-(-\/(_))) =>
              val (h, t) = U(p.t3)
              (h.map(v => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(-\/(v))))), Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((p.t1, p.t2, t, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11)))

            case \/-(\/-(\/-(-\/(_)))) =>
              val (h, t) = U(p.t4)
              (h.map(v => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(-\/(v)))))), Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((p.t1, p.t2, p.t3, t, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11)))

            case \/-(\/-(\/-(\/-(-\/(_))))) =>
              val (h, t) = U(p.t5)
              (h.map(v => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(-\/(v))))))), Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((p.t1, p.t2, p.t3, p.t4, t, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11)))

            case \/-(\/-(\/-(\/-(\/-(-\/(_)))))) =>
              val (h, t) = U(p.t6)
              (h.map(v => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(-\/(v)))))))), Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((p.t1, p.t2, p.t3, p.t4, p.t5, t, p.t7, p.t8, p.t9, p.t10, p.t11)))

            case \/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))) =>
              val (h, t) = U(p.t7)
              (h.map(v => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(-\/(v))))))))), Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, t, p.t8, p.t9, p.t10, p.t11)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))) =>
              val (h, t) = U(p.t8)
              (h.map(v => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v)))))))))), Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, t, p.t9, p.t10, p.t11)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))) =>
              val (h, t) = U(p.t9)
              (h.map(v => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v))))))))))), Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, t, p.t10, p.t11)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))))) =>
              val (h, t) = U(p.t10)
              (h.map(v => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v)))))))))))), Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, t, p.t11)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(_)))))))))) =>
              val (h, t) = U(p.t11)
              (h.map(v => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(v)))))))))))), Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, t)))

          }
      }

    def Prod11TupleIso[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: IsoSet[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11])] =
      IsoSet((_: Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]).run, Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](_: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11])))

    implicit def Prod11Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11])]): Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      Monoid.fromIso(Prod11TupleIso[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11])(M)

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A1]] = {
      val t = M.zero
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((x, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11)))
    }

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A2]] = {
      val t = M.zero
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t.t1, x, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11)))
    }

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A3]] = {
      val t = M.zero
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t.t1, t.t2, x, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11)))
    }

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A4]] = {
      val t = M.zero
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t.t1, t.t2, t.t3, x, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11)))
    }

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A5]] = {
      val t = M.zero
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t.t1, t.t2, t.t3, t.t4, x, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11)))
    }

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A6]] = {
      val t = M.zero
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t.t1, t.t2, t.t3, t.t4, t.t5, x, t.t7, t.t8, t.t9, t.t10, t.t11)))
    }

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A7]] = {
      val t = M.zero
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, x, t.t8, t.t9, t.t10, t.t11)))
    }

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A8]] = {
      val t = M.zero
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, x, t.t9, t.t10, t.t11)))
    }

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A9]] = {
      val t = M.zero
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, x, t.t10, t.t11)))
    }

    implicit def lifta9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A10]] = {
      val t = M.zero
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, x, t.t11)))
    }

    implicit def lifta10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A11]] = {
      val t = M.zero
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, x)))
    }

    implicit def Prod11Lens0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A1]] =
      Lens(p => StoreT.store[F[A1], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](p.t1)(x =>
        Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((x, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11))))

    implicit def Prod11Lens1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A2]] =
      Lens(p => StoreT.store[F[A2], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](p.t2)(x =>
        Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((p.t1, x, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11))))

    implicit def Prod11Lens2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A3]] =
      Lens(p => StoreT.store[F[A3], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](p.t3)(x =>
        Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((p.t1, p.t2, x, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11))))

    implicit def Prod11Lens3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A4]] =
      Lens(p => StoreT.store[F[A4], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](p.t4)(x =>
        Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((p.t1, p.t2, p.t3, x, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11))))

    implicit def Prod11Lens4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A5]] =
      Lens(p => StoreT.store[F[A5], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](p.t5)(x =>
        Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((p.t1, p.t2, p.t3, p.t4, x, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11))))

    implicit def Prod11Lens5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A6]] =
      Lens(p => StoreT.store[F[A6], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](p.t6)(x =>
        Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((p.t1, p.t2, p.t3, p.t4, p.t5, x, p.t7, p.t8, p.t9, p.t10, p.t11))))

    implicit def Prod11Lens6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A7]] =
      Lens(p => StoreT.store[F[A7], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](p.t7)(x =>
        Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, x, p.t8, p.t9, p.t10, p.t11))))

    implicit def Prod11Lens7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A8]] =
      Lens(p => StoreT.store[F[A8], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](p.t8)(x =>
        Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, x, p.t9, p.t10, p.t11))))

    implicit def Prod11Lens8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A9]] =
      Lens(p => StoreT.store[F[A9], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](p.t9)(x =>
        Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, x, p.t10, p.t11))))

    implicit def Prod11Lens9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A10]] =
      Lens(p => StoreT.store[F[A10], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](p.t10)(x =>
        Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, x, p.t11))))

    implicit def Prod11Lens10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A11]] =
      Lens(p => StoreT.store[F[A11], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](p.t11)(x =>
        Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, x))))

  }

  object Prod11 extends Prod11LP {

    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A1] =
      lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A2] =
      lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A3] =
      lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A4] =
      lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A5] =
      lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A6] =
      lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A7] =
      lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A8] =
      lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A9] =
      lifta8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A10] =
      lifta9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A11] =
      lifta10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Prod11Lens0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A1] =
      Prod11Lens0[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Prod11Lens1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A2] =
      Prod11Lens1[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Prod11Lens2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A3] =
      Prod11Lens2[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Prod11Lens3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A4] =
      Prod11Lens3[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Prod11Lens4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A5] =
      Prod11Lens4[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Prod11Lens5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A6] =
      Prod11Lens5[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Prod11Lens6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A7] =
      Prod11Lens6[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Prod11Lens7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A8] =
      Prod11Lens7[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Prod11Lens8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A9] =
      Prod11Lens8[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Prod11Lens9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A10] =
      Prod11Lens9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Prod11Lens10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Lens[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A11] =
      Prod11Lens10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

  }

  @newtype case class Cop11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ F[A11]))))))))))) {
    private def mapN = new Map11C[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11]] {}

    def map1[B](f: F[A1] => F[B]): Cop11[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] =
      Cop11[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Cop11[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] =
      Cop11[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Cop11[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11] =
      Cop11[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Cop11[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11] =
      Cop11[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11](mapN.mapAt(f)(run))

    def map3[B](f: F[A3] => F[B]): Cop11[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11] =
      Cop11[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11](mapN.map3(run)(f))

    def mapAt[B](f: F[A3] => F[B])(implicit d: Dummy3): Cop11[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11] =
      Cop11[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11](mapN.mapAt(f)(run))

    def map4[B](f: F[A4] => F[B]): Cop11[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11] =
      Cop11[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11](mapN.map4(run)(f))

    def mapAt[B](f: F[A4] => F[B])(implicit d: Dummy4): Cop11[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11] =
      Cop11[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11](mapN.mapAt(f)(run))

    def map5[B](f: F[A5] => F[B]): Cop11[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11] =
      Cop11[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11](mapN.map5(run)(f))

    def mapAt[B](f: F[A5] => F[B])(implicit d: Dummy5): Cop11[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11] =
      Cop11[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11](mapN.mapAt(f)(run))

    def map6[B](f: F[A6] => F[B]): Cop11[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11] =
      Cop11[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11](mapN.map6(run)(f))

    def mapAt[B](f: F[A6] => F[B])(implicit d: Dummy6): Cop11[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11] =
      Cop11[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11](mapN.mapAt(f)(run))

    def map7[B](f: F[A7] => F[B]): Cop11[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11] =
      Cop11[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11](mapN.map7(run)(f))

    def mapAt[B](f: F[A7] => F[B])(implicit d: Dummy7): Cop11[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11] =
      Cop11[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11](mapN.mapAt(f)(run))

    def map8[B](f: F[A8] => F[B]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11] =
      Cop11[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11](mapN.map8(run)(f))

    def mapAt[B](f: F[A8] => F[B])(implicit d: Dummy8): Cop11[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11] =
      Cop11[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11](mapN.mapAt(f)(run))

    def map9[B](f: F[A9] => F[B]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11] =
      Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11](mapN.map9(run)(f))

    def mapAt[B](f: F[A9] => F[B])(implicit d: Dummy9): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11] =
      Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11](mapN.mapAt(f)(run))

    def map10[B](f: F[A10] => F[B]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11] =
      Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11](mapN.map10(run)(f))

    def mapAt[B](f: F[A10] => F[B])(implicit d: Dummy10): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11] =
      Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11](mapN.mapAt(f)(run))

    def map11[B](f: F[A11] => F[B]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B] =
      Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B](mapN.map11(run)(f))

    def mapAt[B](f: F[A11] => F[B])(implicit d: Dummy11): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B] =
      Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B](mapN.mapAt(f)(run))

  }

  trait Cop11LP {

    implicit def Cop11Instance[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: FFunctor[Cop11[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] with FTraverseCop[Cop11[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      new FFunctor[Cop11[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] with FTraverseCop[Cop11[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] {
        def map[F[_], G[_]](c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11])(nt: F ~> G): Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] =
          Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](c.run.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_))))))))))))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11])(f: F ~> Lambda[a => A[G[a]]]): A[Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
          c.run match {

            case -\/(x) => Functor[A].map(f(x))(y => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](-\/(y)))

            case \/-(-\/(x)) => Functor[A].map(f(x))(y => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(-\/(y))))

            case \/-(\/-(-\/(x))) => Functor[A].map(f(x))(y => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(-\/(y)))))

            case \/-(\/-(\/-(-\/(x)))) => Functor[A].map(f(x))(y => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(-\/(y))))))

            case \/-(\/-(\/-(\/-(-\/(x))))) => Functor[A].map(f(x))(y => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(-\/(y)))))))

            case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Functor[A].map(f(x))(y => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(-\/(y))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Functor[A].map(f(x))(y => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Functor[A].map(f(x))(y => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Functor[A].map(f(x))(y => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Functor[A].map(f(x))(y => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))) => Functor[A].map(f(x))(y => Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(y))))))))))))

          }
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A1]] =
      Inj.instance(x => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](-\/(x)))

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A2]] =
      Inj.instance(x => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(-\/(x))))

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A3]] =
      Inj.instance(x => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(-\/(x)))))

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A4]] =
      Inj.instance(x => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(-\/(x))))))

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A5]] =
      Inj.instance(x => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(-\/(x)))))))

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A6]] =
      Inj.instance(x => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(-\/(x))))))))

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A7]] =
      Inj.instance(x => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A8]] =
      Inj.instance(x => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A9]] =
      Inj.instance(x => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))

    implicit def inja9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A10]] =
      Inj.instance(x => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))

    implicit def inja10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A11]] =
      Inj.instance(x => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))

    implicit def Cop11PLens0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A1]] =
      PLens(c => c.run match {
        case -\/(x) => Some(StoreT.store[F[A1], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](x)(y => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](-\/(y))))
        case _ => None
      })

    implicit def Cop11PLens1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A2]] =
      PLens(c => c.run match {
        case \/-(-\/(x)) => Some(StoreT.store[F[A2], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](x)(y => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(-\/(y)))))
        case _ => None
      })

    implicit def Cop11PLens2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A3]] =
      PLens(c => c.run match {
        case \/-(\/-(-\/(x))) => Some(StoreT.store[F[A3], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](x)(y => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(-\/(y))))))
        case _ => None
      })

    implicit def Cop11PLens3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A4]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(StoreT.store[F[A4], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](x)(y => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(-\/(y)))))))
        case _ => None
      })

    implicit def Cop11PLens4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A5]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(StoreT.store[F[A5], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](x)(y => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(-\/(y))))))))
        case _ => None
      })

    implicit def Cop11PLens5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A6]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(StoreT.store[F[A6], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](x)(y => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))
        case _ => None
      })

    implicit def Cop11PLens6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A7]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(StoreT.store[F[A7], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](x)(y => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))
        case _ => None
      })

    implicit def Cop11PLens7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A8]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(StoreT.store[F[A8], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](x)(y => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))
        case _ => None
      })

    implicit def Cop11PLens8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A9]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(StoreT.store[F[A9], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](x)(y => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))
        case _ => None
      })

    implicit def Cop11PLens9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A10]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(StoreT.store[F[A10], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](x)(y => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))
        case _ => None
      })

    implicit def Cop11PLens10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A11]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))) => Some(StoreT.store[F[A11], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](x)(y => Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(y)))))))))))))
        case _ => None
      })

  }

  object Cop11 extends Cop11LP {

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A1] =
      inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A2] =
      inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A3] =
      inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A4] =
      inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A5] =
      inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A6] =
      inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A7] =
      inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A8] =
      inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A9] =
      inja8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A10] =
      inja9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A11] =
      inja10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Cop11PLens0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A1] =
      Cop11PLens0[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Cop11PLens1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A2] =
      Cop11PLens1[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Cop11PLens2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A3] =
      Cop11PLens2[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Cop11PLens3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A4] =
      Cop11PLens3[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Cop11PLens4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A5] =
      Cop11PLens4[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Cop11PLens5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A6] =
      Cop11PLens5[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Cop11PLens6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A7] =
      Cop11PLens6[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Cop11PLens7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A8] =
      Cop11PLens7[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Cop11PLens8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A9] =
      Cop11PLens8[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Cop11PLens9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A10] =
      Cop11PLens9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def Cop11PLens10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: PLens[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A11] =
      Cop11PLens10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

  }
}
