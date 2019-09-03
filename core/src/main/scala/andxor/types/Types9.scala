package andxor.types

import andxor._
import scalaz.{\/, -\/, \/-, ~>, Applicative, Functor, Lens, Monoid, PLens, PlusEmpty, StoreT}
import scalaz.Id.Id
import scalaz.Isomorphism.IsoSet

trait Types9 {
  @newtype case class Prod9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9])) { self =>
    def t1: F[A1] = run._1
    def t2: F[A2] = run._2
    def t3: F[A3] = run._3
    def t4: F[A4] = run._4
    def t5: F[A5] = run._5
    def t6: F[A6] = run._6
    def t7: F[A7] = run._7
    def t8: F[A8] = run._8
    def t9: F[A9] = run._9

    private def mapN = new Map9P[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9]] {}

    def map1[B](f: F[A1] => F[B]): Prod9[F, B, A2, A3, A4, A5, A6, A7, A8, A9] =
      Prod9[F, B, A2, A3, A4, A5, A6, A7, A8, A9](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Prod9[F, B, A2, A3, A4, A5, A6, A7, A8, A9] =
      Prod9[F, B, A2, A3, A4, A5, A6, A7, A8, A9](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Prod9[F, A1, B, A3, A4, A5, A6, A7, A8, A9] =
      Prod9[F, A1, B, A3, A4, A5, A6, A7, A8, A9](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Prod9[F, A1, B, A3, A4, A5, A6, A7, A8, A9] =
      Prod9[F, A1, B, A3, A4, A5, A6, A7, A8, A9](mapN.mapAt(f)(run))

    def map3[B](f: F[A3] => F[B]): Prod9[F, A1, A2, B, A4, A5, A6, A7, A8, A9] =
      Prod9[F, A1, A2, B, A4, A5, A6, A7, A8, A9](mapN.map3(run)(f))

    def mapAt[B](f: F[A3] => F[B])(implicit d: Dummy3): Prod9[F, A1, A2, B, A4, A5, A6, A7, A8, A9] =
      Prod9[F, A1, A2, B, A4, A5, A6, A7, A8, A9](mapN.mapAt(f)(run))

    def map4[B](f: F[A4] => F[B]): Prod9[F, A1, A2, A3, B, A5, A6, A7, A8, A9] =
      Prod9[F, A1, A2, A3, B, A5, A6, A7, A8, A9](mapN.map4(run)(f))

    def mapAt[B](f: F[A4] => F[B])(implicit d: Dummy4): Prod9[F, A1, A2, A3, B, A5, A6, A7, A8, A9] =
      Prod9[F, A1, A2, A3, B, A5, A6, A7, A8, A9](mapN.mapAt(f)(run))

    def map5[B](f: F[A5] => F[B]): Prod9[F, A1, A2, A3, A4, B, A6, A7, A8, A9] =
      Prod9[F, A1, A2, A3, A4, B, A6, A7, A8, A9](mapN.map5(run)(f))

    def mapAt[B](f: F[A5] => F[B])(implicit d: Dummy5): Prod9[F, A1, A2, A3, A4, B, A6, A7, A8, A9] =
      Prod9[F, A1, A2, A3, A4, B, A6, A7, A8, A9](mapN.mapAt(f)(run))

    def map6[B](f: F[A6] => F[B]): Prod9[F, A1, A2, A3, A4, A5, B, A7, A8, A9] =
      Prod9[F, A1, A2, A3, A4, A5, B, A7, A8, A9](mapN.map6(run)(f))

    def mapAt[B](f: F[A6] => F[B])(implicit d: Dummy6): Prod9[F, A1, A2, A3, A4, A5, B, A7, A8, A9] =
      Prod9[F, A1, A2, A3, A4, A5, B, A7, A8, A9](mapN.mapAt(f)(run))

    def map7[B](f: F[A7] => F[B]): Prod9[F, A1, A2, A3, A4, A5, A6, B, A8, A9] =
      Prod9[F, A1, A2, A3, A4, A5, A6, B, A8, A9](mapN.map7(run)(f))

    def mapAt[B](f: F[A7] => F[B])(implicit d: Dummy7): Prod9[F, A1, A2, A3, A4, A5, A6, B, A8, A9] =
      Prod9[F, A1, A2, A3, A4, A5, A6, B, A8, A9](mapN.mapAt(f)(run))

    def map8[B](f: F[A8] => F[B]): Prod9[F, A1, A2, A3, A4, A5, A6, A7, B, A9] =
      Prod9[F, A1, A2, A3, A4, A5, A6, A7, B, A9](mapN.map8(run)(f))

    def mapAt[B](f: F[A8] => F[B])(implicit d: Dummy8): Prod9[F, A1, A2, A3, A4, A5, A6, A7, B, A9] =
      Prod9[F, A1, A2, A3, A4, A5, A6, A7, B, A9](mapN.mapAt(f)(run))

    def map9[B](f: F[A9] => F[B]): Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, B] =
      Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, B](mapN.map9(run)(f))

    def mapAt[B](f: F[A9] => F[B])(implicit d: Dummy9): Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, B] =
      Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, B](mapN.mapAt(f)(run))

  }

  trait Prod9LP {

    implicit def Prod9Instance[A1, A2, A3, A4, A5, A6, A7, A8, A9]: FFunctor[Prod9[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]] with FTraverseProd[Prod9[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
      new FFunctor[Prod9[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]] with FTraverseProd[Prod9[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]] {
        def map[F[_], G[_]](p: Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9])(nt: F ~> G): Prod9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9] =
          Prod9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9]((nt(p.t1), nt(p.t2), nt(p.t3), nt(p.t4), nt(p.t5), nt(p.t6), nt(p.t7), nt(p.t8), nt(p.t9)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9])(f: F ~> Lambda[a => A[G[a]]]): A[Prod9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
          Applicative[A].ap(f(p.t9))(Applicative[A].ap(f(p.t8))(Applicative[A].ap(f(p.t7))(Applicative[A].ap(f(p.t6))(Applicative[A].ap(f(p.t5))(Applicative[A].ap(f(p.t4))(Applicative[A].ap(f(p.t3))(Applicative[A].ap(f(p.t2))(Applicative[A].map(f(p.t1))((i0: G[A1]) => (i1: G[A2]) => (i2: G[A3]) => (i3: G[A4]) => (i4: G[A5]) => (i5: G[A6]) => (i6: G[A7]) => (i7: G[A8]) => (i8: G[A9]) => Prod9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9]((i0, i1, i2, i3, i4, i5, i6, i7, i8)))))))))))
      }

    implicit def Prod9FoldMap[A1, A2, A3, A4, A5, A6, A7, A8, A9]: FoldMap[Prod9[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9], Cop9[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
      new FoldMap[Prod9[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9], Cop9[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]] {
        def emptyProd[F[_]](implicit PE: PlusEmpty[F]): Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] =
          Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5], PE.empty[A6], PE.empty[A7], PE.empty[A8], PE.empty[A9]))

        def unconsAll[F[_], G[_]](p: Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9])(implicit U: Uncons[F, G]): (List[Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9]], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]) = {
          val (h1, t1) = U(p.t1)
          val (h2, t2) = U(p.t2)
          val (h3, t3) = U(p.t3)
          val (h4, t4) = U(p.t4)
          val (h5, t5) = U(p.t5)
          val (h6, t6) = U(p.t6)
          val (h7, t7) = U(p.t7)
          val (h8, t8) = U(p.t8)
          val (h9, t9) = U(p.t9)
          (
            List(h1.map(Inj[Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9], G[A1]].apply(_)), h2.map(Inj[Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9], G[A2]].apply(_)), h3.map(Inj[Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9], G[A3]].apply(_)), h4.map(Inj[Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9], G[A4]].apply(_)), h5.map(Inj[Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9], G[A5]].apply(_)), h6.map(Inj[Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9], G[A6]].apply(_)), h7.map(Inj[Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9], G[A7]].apply(_)), h8.map(Inj[Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9], G[A8]].apply(_)), h9.map(Inj[Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9], G[A9]].apply(_))).flatten,
            Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t1, t2, t3, t4, t5, t6, t7, t8, t9)))
        }

        def unconsOne[F[_], G[_]](p: Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], c: Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9])(implicit U: Uncons[F, G]): (Option[Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9]], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]) =
          c.run match {

            case -\/(_) =>
              val (h, t) = U(p.t1)
              (h.map(v => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](-\/(v))), Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9)))

            case \/-(-\/(_)) =>
              val (h, t) = U(p.t2)
              (h.map(v => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(-\/(v)))), Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, t, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9)))

            case \/-(\/-(-\/(_))) =>
              val (h, t) = U(p.t3)
              (h.map(v => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(-\/(v))))), Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, t, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9)))

            case \/-(\/-(\/-(-\/(_)))) =>
              val (h, t) = U(p.t4)
              (h.map(v => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(-\/(v)))))), Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, t, p.t5, p.t6, p.t7, p.t8, p.t9)))

            case \/-(\/-(\/-(\/-(-\/(_))))) =>
              val (h, t) = U(p.t5)
              (h.map(v => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(-\/(v))))))), Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, p.t4, t, p.t6, p.t7, p.t8, p.t9)))

            case \/-(\/-(\/-(\/-(\/-(-\/(_)))))) =>
              val (h, t) = U(p.t6)
              (h.map(v => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(-\/(v)))))))), Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, p.t4, p.t5, t, p.t7, p.t8, p.t9)))

            case \/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))) =>
              val (h, t) = U(p.t7)
              (h.map(v => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(\/-(-\/(v))))))))), Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, t, p.t8, p.t9)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))) =>
              val (h, t) = U(p.t8)
              (h.map(v => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(v)))))))))), Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, t, p.t9)))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(_)))))))) =>
              val (h, t) = U(p.t9)
              (h.map(v => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(v)))))))))), Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, t)))

          }
      }

    def Prod9TupleIso[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: IsoSet[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9])] =
      IsoSet((_: Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]).run, Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](_: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9])))

    implicit def Prod9Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9])]): Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
      Monoid.fromIso(Prod9TupleIso[F, A1, A2, A3, A4, A5, A6, A7, A8, A9])(M)

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A1]] = {
      val t = M.zero
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((x, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9)))
    }

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A2]] = {
      val t = M.zero
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t.t1, x, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9)))
    }

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A3]] = {
      val t = M.zero
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t.t1, t.t2, x, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9)))
    }

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A4]] = {
      val t = M.zero
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t.t1, t.t2, t.t3, x, t.t5, t.t6, t.t7, t.t8, t.t9)))
    }

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A5]] = {
      val t = M.zero
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t.t1, t.t2, t.t3, t.t4, x, t.t6, t.t7, t.t8, t.t9)))
    }

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A6]] = {
      val t = M.zero
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t.t1, t.t2, t.t3, t.t4, t.t5, x, t.t7, t.t8, t.t9)))
    }

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A7]] = {
      val t = M.zero
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, x, t.t8, t.t9)))
    }

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A8]] = {
      val t = M.zero
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, x, t.t9)))
    }

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A9]] = {
      val t = M.zero
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, x)))
    }

    implicit def Prod9Lens0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A1]] =
      Lens(p => StoreT.store[F[A1], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]](p.t1)(x =>
        Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((x, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9))))

    implicit def Prod9Lens1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A2]] =
      Lens(p => StoreT.store[F[A2], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]](p.t2)(x =>
        Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, x, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9))))

    implicit def Prod9Lens2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A3]] =
      Lens(p => StoreT.store[F[A3], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]](p.t3)(x =>
        Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, x, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9))))

    implicit def Prod9Lens3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A4]] =
      Lens(p => StoreT.store[F[A4], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]](p.t4)(x =>
        Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, x, p.t5, p.t6, p.t7, p.t8, p.t9))))

    implicit def Prod9Lens4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A5]] =
      Lens(p => StoreT.store[F[A5], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]](p.t5)(x =>
        Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, p.t4, x, p.t6, p.t7, p.t8, p.t9))))

    implicit def Prod9Lens5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A6]] =
      Lens(p => StoreT.store[F[A6], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]](p.t6)(x =>
        Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, p.t4, p.t5, x, p.t7, p.t8, p.t9))))

    implicit def Prod9Lens6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A7]] =
      Lens(p => StoreT.store[F[A7], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]](p.t7)(x =>
        Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, x, p.t8, p.t9))))

    implicit def Prod9Lens7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A8]] =
      Lens(p => StoreT.store[F[A8], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]](p.t8)(x =>
        Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, x, p.t9))))

    implicit def Prod9Lens8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A9]] =
      Lens(p => StoreT.store[F[A9], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]](p.t9)(x =>
        Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, x))))

  }

  object Prod9 extends Prod9LP {

    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A1] =
      lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A2] =
      lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A3] =
      lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A4] =
      lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A5] =
      lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A6] =
      lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A7] =
      lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A8] =
      lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A9] =
      lifta8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Prod9Lens0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A1] =
      Prod9Lens0[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Prod9Lens1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A2] =
      Prod9Lens1[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Prod9Lens2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A3] =
      Prod9Lens2[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Prod9Lens3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A4] =
      Prod9Lens3[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Prod9Lens4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A5] =
      Prod9Lens4[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Prod9Lens5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A6] =
      Prod9Lens5[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Prod9Lens6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A7] =
      Prod9Lens6[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Prod9Lens7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A8] =
      Prod9Lens7[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Prod9Lens8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A9] =
      Prod9Lens8[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

  }

  @newtype case class Cop9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ F[A9]))))))))) {
    private def mapN = new Map9C[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9]] {}

    def map1[B](f: F[A1] => F[B]): Cop9[F, B, A2, A3, A4, A5, A6, A7, A8, A9] =
      Cop9[F, B, A2, A3, A4, A5, A6, A7, A8, A9](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Cop9[F, B, A2, A3, A4, A5, A6, A7, A8, A9] =
      Cop9[F, B, A2, A3, A4, A5, A6, A7, A8, A9](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Cop9[F, A1, B, A3, A4, A5, A6, A7, A8, A9] =
      Cop9[F, A1, B, A3, A4, A5, A6, A7, A8, A9](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Cop9[F, A1, B, A3, A4, A5, A6, A7, A8, A9] =
      Cop9[F, A1, B, A3, A4, A5, A6, A7, A8, A9](mapN.mapAt(f)(run))

    def map3[B](f: F[A3] => F[B]): Cop9[F, A1, A2, B, A4, A5, A6, A7, A8, A9] =
      Cop9[F, A1, A2, B, A4, A5, A6, A7, A8, A9](mapN.map3(run)(f))

    def mapAt[B](f: F[A3] => F[B])(implicit d: Dummy3): Cop9[F, A1, A2, B, A4, A5, A6, A7, A8, A9] =
      Cop9[F, A1, A2, B, A4, A5, A6, A7, A8, A9](mapN.mapAt(f)(run))

    def map4[B](f: F[A4] => F[B]): Cop9[F, A1, A2, A3, B, A5, A6, A7, A8, A9] =
      Cop9[F, A1, A2, A3, B, A5, A6, A7, A8, A9](mapN.map4(run)(f))

    def mapAt[B](f: F[A4] => F[B])(implicit d: Dummy4): Cop9[F, A1, A2, A3, B, A5, A6, A7, A8, A9] =
      Cop9[F, A1, A2, A3, B, A5, A6, A7, A8, A9](mapN.mapAt(f)(run))

    def map5[B](f: F[A5] => F[B]): Cop9[F, A1, A2, A3, A4, B, A6, A7, A8, A9] =
      Cop9[F, A1, A2, A3, A4, B, A6, A7, A8, A9](mapN.map5(run)(f))

    def mapAt[B](f: F[A5] => F[B])(implicit d: Dummy5): Cop9[F, A1, A2, A3, A4, B, A6, A7, A8, A9] =
      Cop9[F, A1, A2, A3, A4, B, A6, A7, A8, A9](mapN.mapAt(f)(run))

    def map6[B](f: F[A6] => F[B]): Cop9[F, A1, A2, A3, A4, A5, B, A7, A8, A9] =
      Cop9[F, A1, A2, A3, A4, A5, B, A7, A8, A9](mapN.map6(run)(f))

    def mapAt[B](f: F[A6] => F[B])(implicit d: Dummy6): Cop9[F, A1, A2, A3, A4, A5, B, A7, A8, A9] =
      Cop9[F, A1, A2, A3, A4, A5, B, A7, A8, A9](mapN.mapAt(f)(run))

    def map7[B](f: F[A7] => F[B]): Cop9[F, A1, A2, A3, A4, A5, A6, B, A8, A9] =
      Cop9[F, A1, A2, A3, A4, A5, A6, B, A8, A9](mapN.map7(run)(f))

    def mapAt[B](f: F[A7] => F[B])(implicit d: Dummy7): Cop9[F, A1, A2, A3, A4, A5, A6, B, A8, A9] =
      Cop9[F, A1, A2, A3, A4, A5, A6, B, A8, A9](mapN.mapAt(f)(run))

    def map8[B](f: F[A8] => F[B]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, B, A9] =
      Cop9[F, A1, A2, A3, A4, A5, A6, A7, B, A9](mapN.map8(run)(f))

    def mapAt[B](f: F[A8] => F[B])(implicit d: Dummy8): Cop9[F, A1, A2, A3, A4, A5, A6, A7, B, A9] =
      Cop9[F, A1, A2, A3, A4, A5, A6, A7, B, A9](mapN.mapAt(f)(run))

    def map9[B](f: F[A9] => F[B]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, B] =
      Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, B](mapN.map9(run)(f))

    def mapAt[B](f: F[A9] => F[B])(implicit d: Dummy9): Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, B] =
      Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, B](mapN.mapAt(f)(run))

  }

  trait Cop9LP {

    implicit def Cop9Instance[A1, A2, A3, A4, A5, A6, A7, A8, A9]: FFunctor[Cop9[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]] with FTraverseCop[Cop9[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
      new FFunctor[Cop9[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]] with FTraverseCop[Cop9[?[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]] {
        def map[F[_], G[_]](c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9])(nt: F ~> G): Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9] =
          Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](c.run.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_))))))))))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9])(f: F ~> Lambda[a => A[G[a]]]): A[Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
          c.run match {

            case -\/(x) => Functor[A].map(f(x))(y => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](-\/(y)))

            case \/-(-\/(x)) => Functor[A].map(f(x))(y => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(-\/(y))))

            case \/-(\/-(-\/(x))) => Functor[A].map(f(x))(y => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(-\/(y)))))

            case \/-(\/-(\/-(-\/(x)))) => Functor[A].map(f(x))(y => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(-\/(y))))))

            case \/-(\/-(\/-(\/-(-\/(x))))) => Functor[A].map(f(x))(y => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(-\/(y)))))))

            case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Functor[A].map(f(x))(y => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(-\/(y))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Functor[A].map(f(x))(y => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Functor[A].map(f(x))(y => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))

            case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))) => Functor[A].map(f(x))(y => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(y))))))))))

          }
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A1]] =
      Inj.instance(x => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](-\/(x)))

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A2]] =
      Inj.instance(x => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(-\/(x))))

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A3]] =
      Inj.instance(x => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(-\/(x)))))

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A4]] =
      Inj.instance(x => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(-\/(x))))))

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A5]] =
      Inj.instance(x => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(-\/(x)))))))

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A6]] =
      Inj.instance(x => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(-\/(x))))))))

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A7]] =
      Inj.instance(x => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A8]] =
      Inj.instance(x => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A9]] =
      Inj.instance(x => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))

    implicit def Cop9PLens0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: PLens[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A1]] =
      PLens(c => c.run match {
        case -\/(x) => Some(StoreT.store[F[A1], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]](x)(y => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](-\/(y))))
        case _ => None
      })

    implicit def Cop9PLens1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: PLens[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A2]] =
      PLens(c => c.run match {
        case \/-(-\/(x)) => Some(StoreT.store[F[A2], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]](x)(y => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(-\/(y)))))
        case _ => None
      })

    implicit def Cop9PLens2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: PLens[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A3]] =
      PLens(c => c.run match {
        case \/-(\/-(-\/(x))) => Some(StoreT.store[F[A3], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]](x)(y => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(-\/(y))))))
        case _ => None
      })

    implicit def Cop9PLens3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: PLens[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A4]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(StoreT.store[F[A4], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]](x)(y => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(-\/(y)))))))
        case _ => None
      })

    implicit def Cop9PLens4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: PLens[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A5]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(StoreT.store[F[A5], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]](x)(y => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(-\/(y))))))))
        case _ => None
      })

    implicit def Cop9PLens5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: PLens[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A6]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(StoreT.store[F[A6], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]](x)(y => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))
        case _ => None
      })

    implicit def Cop9PLens6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: PLens[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A7]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(StoreT.store[F[A7], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]](x)(y => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))
        case _ => None
      })

    implicit def Cop9PLens7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: PLens[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A8]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(StoreT.store[F[A8], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]](x)(y => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))
        case _ => None
      })

    implicit def Cop9PLens8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: PLens[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A9]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))) => Some(StoreT.store[F[A9], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]](x)(y => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(y)))))))))))
        case _ => None
      })

  }

  object Cop9 extends Cop9LP {

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A1] =
      inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A2] =
      inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A3] =
      inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A4] =
      inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A5] =
      inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A6] =
      inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A7] =
      inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A8] =
      inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A9] =
      inja8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Cop9PLens0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: PLens[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A1] =
      Cop9PLens0[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Cop9PLens1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: PLens[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A2] =
      Cop9PLens1[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Cop9PLens2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: PLens[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A3] =
      Cop9PLens2[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Cop9PLens3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: PLens[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A4] =
      Cop9PLens3[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Cop9PLens4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: PLens[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A5] =
      Cop9PLens4[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Cop9PLens5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: PLens[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A6] =
      Cop9PLens5[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Cop9PLens6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: PLens[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A7] =
      Cop9PLens6[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Cop9PLens7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: PLens[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A8] =
      Cop9PLens7[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Cop9PLens8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: PLens[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A9] =
      Cop9PLens8[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

  }
}
