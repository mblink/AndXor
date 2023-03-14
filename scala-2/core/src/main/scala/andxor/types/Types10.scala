package andxor.types

import andxor._
import monocle.{Lens, Optional}
import cats.{~>, Applicative, Functor, Id, Monoid, MonoidK}
import cats.syntax.either._
import cats.syntax.invariant._
import monocle.Iso

trait Types10 {
  @newtype case class Prod10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10])) { self =>
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

    private def mapN = new Map10P[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10]] {}

    def map1[B](f: F[A1] => F[B]): Prod10[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10] =
      Prod10[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Prod10[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10] =
      Prod10[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Prod10[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10] =
      Prod10[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Prod10[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10] =
      Prod10[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10](mapN.mapAt(f)(run))

    def map3[B](f: F[A3] => F[B]): Prod10[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10] =
      Prod10[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10](mapN.map3(run)(f))

    def mapAt[B](f: F[A3] => F[B])(implicit d: Dummy3): Prod10[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10] =
      Prod10[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10](mapN.mapAt(f)(run))

    def map4[B](f: F[A4] => F[B]): Prod10[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10] =
      Prod10[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10](mapN.map4(run)(f))

    def mapAt[B](f: F[A4] => F[B])(implicit d: Dummy4): Prod10[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10] =
      Prod10[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10](mapN.mapAt(f)(run))

    def map5[B](f: F[A5] => F[B]): Prod10[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10] =
      Prod10[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10](mapN.map5(run)(f))

    def mapAt[B](f: F[A5] => F[B])(implicit d: Dummy5): Prod10[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10] =
      Prod10[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10](mapN.mapAt(f)(run))

    def map6[B](f: F[A6] => F[B]): Prod10[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10] =
      Prod10[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10](mapN.map6(run)(f))

    def mapAt[B](f: F[A6] => F[B])(implicit d: Dummy6): Prod10[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10] =
      Prod10[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10](mapN.mapAt(f)(run))

    def map7[B](f: F[A7] => F[B]): Prod10[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10] =
      Prod10[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10](mapN.map7(run)(f))

    def mapAt[B](f: F[A7] => F[B])(implicit d: Dummy7): Prod10[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10] =
      Prod10[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10](mapN.mapAt(f)(run))

    def map8[B](f: F[A8] => F[B]): Prod10[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10] =
      Prod10[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10](mapN.map8(run)(f))

    def mapAt[B](f: F[A8] => F[B])(implicit d: Dummy8): Prod10[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10] =
      Prod10[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10](mapN.mapAt(f)(run))

    def map9[B](f: F[A9] => F[B]): Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10] =
      Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10](mapN.map9(run)(f))

    def mapAt[B](f: F[A9] => F[B])(implicit d: Dummy9): Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10] =
      Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10](mapN.mapAt(f)(run))

    def map10[B](f: F[A10] => F[B]): Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B] =
      Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B](mapN.map10(run)(f))

    def mapAt[B](f: F[A10] => F[B])(implicit d: Dummy10): Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B] =
      Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B](mapN.mapAt(f)(run))

  }

  trait Prod10LP {

    implicit def Prod10Instance[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: FFunctor[Prod10[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] with FTraverseProd[Prod10[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
      new FFunctor[Prod10[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] with FTraverseProd[Prod10[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] {
        def map[F[_], G[_]](p: Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10])(nt: F ~> G): Prod10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] =
          Prod10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((nt(p.t1), nt(p.t2), nt(p.t3), nt(p.t4), nt(p.t5), nt(p.t6), nt(p.t7), nt(p.t8), nt(p.t9), nt(p.t10)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10])(f: F ~> Lambda[a => A[G[a]]]): A[Prod10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
          Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].map(f(p.t1))((i0: G[A1]) => (i1: G[A2]) => (i2: G[A3]) => (i3: G[A4]) => (i4: G[A5]) => (i5: G[A6]) => (i6: G[A7]) => (i7: G[A8]) => (i8: G[A9]) => (i9: G[A10]) => Prod10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9))))(f(p.t2)))(f(p.t3)))(f(p.t4)))(f(p.t5)))(f(p.t6)))(f(p.t7)))(f(p.t8)))(f(p.t9)))(f(p.t10))
      }

    implicit def Prod10FoldMap[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: FoldMap[Prod10[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], Cop10[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
      new FoldMap[Prod10[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], Cop10[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] {
        def emptyProd[F[_]](implicit PE: MonoidK[F]): Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] =
          Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5], PE.empty[A6], PE.empty[A7], PE.empty[A8], PE.empty[A9], PE.empty[A10]))

        def unconsAll[F[_], G[_]](p: Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10])(implicit U: Uncons[F, G]): (List[Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]) = {
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
          (
            List(h1.map(Inj[Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], G[A1]].apply(_)), h2.map(Inj[Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], G[A2]].apply(_)), h3.map(Inj[Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], G[A3]].apply(_)), h4.map(Inj[Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], G[A4]].apply(_)), h5.map(Inj[Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], G[A5]].apply(_)), h6.map(Inj[Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], G[A6]].apply(_)), h7.map(Inj[Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], G[A7]].apply(_)), h8.map(Inj[Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], G[A8]].apply(_)), h9.map(Inj[Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], G[A9]].apply(_)), h10.map(Inj[Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], G[A10]].apply(_))).flatten,
            Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)))
        }

        def unconsOne[F[_], G[_]](p: Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], c: Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10])(implicit U: Uncons[F, G]): (Option[Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]) =
          c.run match {

            case Left(_) =>
              val (h, t) = U(p.t1)
              (h.map(v => Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Left(v))), Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10)))

            case Right(Left(_)) =>
              val (h, t) = U(p.t2)
              (h.map(v => Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Left(v)))), Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((p.t1, t, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10)))

            case Right(Right(Left(_))) =>
              val (h, t) = U(p.t3)
              (h.map(v => Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Left(v))))), Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((p.t1, p.t2, t, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10)))

            case Right(Right(Right(Left(_)))) =>
              val (h, t) = U(p.t4)
              (h.map(v => Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Left(v)))))), Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((p.t1, p.t2, p.t3, t, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10)))

            case Right(Right(Right(Right(Left(_))))) =>
              val (h, t) = U(p.t5)
              (h.map(v => Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Left(v))))))), Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((p.t1, p.t2, p.t3, p.t4, t, p.t6, p.t7, p.t8, p.t9, p.t10)))

            case Right(Right(Right(Right(Right(Left(_)))))) =>
              val (h, t) = U(p.t6)
              (h.map(v => Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Right(Left(v)))))))), Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((p.t1, p.t2, p.t3, p.t4, p.t5, t, p.t7, p.t8, p.t9, p.t10)))

            case Right(Right(Right(Right(Right(Right(Left(_))))))) =>
              val (h, t) = U(p.t7)
              (h.map(v => Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Right(Right(Left(v))))))))), Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, t, p.t8, p.t9, p.t10)))

            case Right(Right(Right(Right(Right(Right(Right(Left(_)))))))) =>
              val (h, t) = U(p.t8)
              (h.map(v => Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Right(Right(Right(Left(v)))))))))), Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, t, p.t9, p.t10)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Left(_))))))))) =>
              val (h, t) = U(p.t9)
              (h.map(v => Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Right(Right(Right(Right(Left(v))))))))))), Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, t, p.t10)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(_))))))))) =>
              val (h, t) = U(p.t10)
              (h.map(v => Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Right(Right(Right(Right(Right(v))))))))))), Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, t)))

          }
      }

    def Prod10TupleIso[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Iso[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10])] =
      Iso((_: Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]).run)(Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](_: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10])))

    implicit def Prod10Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10])]): Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = {
      val iso = Prod10TupleIso[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]
      M.imap(iso.reverseGet)(iso.get)
    }

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A1]] = {
      val t = M.empty
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((x, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10)))
    }

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A2]] = {
      val t = M.empty
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t.t1, x, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10)))
    }

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A3]] = {
      val t = M.empty
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t.t1, t.t2, x, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10)))
    }

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A4]] = {
      val t = M.empty
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t.t1, t.t2, t.t3, x, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10)))
    }

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A5]] = {
      val t = M.empty
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t.t1, t.t2, t.t3, t.t4, x, t.t6, t.t7, t.t8, t.t9, t.t10)))
    }

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A6]] = {
      val t = M.empty
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t.t1, t.t2, t.t3, t.t4, t.t5, x, t.t7, t.t8, t.t9, t.t10)))
    }

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A7]] = {
      val t = M.empty
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, x, t.t8, t.t9, t.t10)))
    }

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A8]] = {
      val t = M.empty
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, x, t.t9, t.t10)))
    }

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A9]] = {
      val t = M.empty
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, x, t.t10)))
    }

    implicit def lifta9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A10]] = {
      val t = M.empty
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, x)))
    }

    implicit def Prod10Lens0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Lens[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A1]] =
      Lens[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A1]](p => p.t1)(x => p =>
        Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((x, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10)))

    implicit def Prod10Lens1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Lens[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A2]] =
      Lens[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A2]](p => p.t2)(x => p =>
        Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((p.t1, x, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10)))

    implicit def Prod10Lens2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Lens[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A3]] =
      Lens[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A3]](p => p.t3)(x => p =>
        Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((p.t1, p.t2, x, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10)))

    implicit def Prod10Lens3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Lens[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A4]] =
      Lens[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A4]](p => p.t4)(x => p =>
        Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((p.t1, p.t2, p.t3, x, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10)))

    implicit def Prod10Lens4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Lens[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A5]] =
      Lens[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A5]](p => p.t5)(x => p =>
        Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((p.t1, p.t2, p.t3, p.t4, x, p.t6, p.t7, p.t8, p.t9, p.t10)))

    implicit def Prod10Lens5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Lens[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A6]] =
      Lens[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A6]](p => p.t6)(x => p =>
        Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((p.t1, p.t2, p.t3, p.t4, p.t5, x, p.t7, p.t8, p.t9, p.t10)))

    implicit def Prod10Lens6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Lens[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A7]] =
      Lens[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A7]](p => p.t7)(x => p =>
        Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, x, p.t8, p.t9, p.t10)))

    implicit def Prod10Lens7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Lens[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A8]] =
      Lens[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A8]](p => p.t8)(x => p =>
        Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, x, p.t9, p.t10)))

    implicit def Prod10Lens8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Lens[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A9]] =
      Lens[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A9]](p => p.t9)(x => p =>
        Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, x, p.t10)))

    implicit def Prod10Lens9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Lens[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A10]] =
      Lens[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A10]](p => p.t10)(x => p =>
        Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, x)))

  }

  object Prod10 extends Prod10LP {

    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A1] =
      lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A2] =
      lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A3] =
      lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A4] =
      lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A5] =
      lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A6] =
      lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A7] =
      lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A8] =
      lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A9] =
      lifta8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A10] =
      lifta9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def Prod10Lens0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Lens[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A1] =
      Prod10Lens0[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def Prod10Lens1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Lens[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A2] =
      Prod10Lens1[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def Prod10Lens2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Lens[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A3] =
      Prod10Lens2[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def Prod10Lens3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Lens[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A4] =
      Prod10Lens3[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def Prod10Lens4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Lens[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A5] =
      Prod10Lens4[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def Prod10Lens5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Lens[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A6] =
      Prod10Lens5[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def Prod10Lens6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Lens[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A7] =
      Prod10Lens6[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def Prod10Lens7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Lens[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A8] =
      Prod10Lens7[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def Prod10Lens8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Lens[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A9] =
      Prod10Lens8[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def Prod10Lens9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Lens[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A10] =
      Prod10Lens9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

  }

  @newtype case class Cop10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](run: Either[F[A1], Either[F[A2], Either[F[A3], Either[F[A4], Either[F[A5], Either[F[A6], Either[F[A7], Either[F[A8], Either[F[A9], F[A10]]]]]]]]]]) {
    private def mapN = new Map10C[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10]] {}

    def map1[B](f: F[A1] => F[B]): Cop10[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10] =
      Cop10[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Cop10[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10] =
      Cop10[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Cop10[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10] =
      Cop10[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Cop10[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10] =
      Cop10[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10](mapN.mapAt(f)(run))

    def map3[B](f: F[A3] => F[B]): Cop10[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10] =
      Cop10[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10](mapN.map3(run)(f))

    def mapAt[B](f: F[A3] => F[B])(implicit d: Dummy3): Cop10[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10] =
      Cop10[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10](mapN.mapAt(f)(run))

    def map4[B](f: F[A4] => F[B]): Cop10[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10] =
      Cop10[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10](mapN.map4(run)(f))

    def mapAt[B](f: F[A4] => F[B])(implicit d: Dummy4): Cop10[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10] =
      Cop10[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10](mapN.mapAt(f)(run))

    def map5[B](f: F[A5] => F[B]): Cop10[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10] =
      Cop10[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10](mapN.map5(run)(f))

    def mapAt[B](f: F[A5] => F[B])(implicit d: Dummy5): Cop10[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10] =
      Cop10[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10](mapN.mapAt(f)(run))

    def map6[B](f: F[A6] => F[B]): Cop10[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10] =
      Cop10[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10](mapN.map6(run)(f))

    def mapAt[B](f: F[A6] => F[B])(implicit d: Dummy6): Cop10[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10] =
      Cop10[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10](mapN.mapAt(f)(run))

    def map7[B](f: F[A7] => F[B]): Cop10[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10] =
      Cop10[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10](mapN.map7(run)(f))

    def mapAt[B](f: F[A7] => F[B])(implicit d: Dummy7): Cop10[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10] =
      Cop10[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10](mapN.mapAt(f)(run))

    def map8[B](f: F[A8] => F[B]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10] =
      Cop10[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10](mapN.map8(run)(f))

    def mapAt[B](f: F[A8] => F[B])(implicit d: Dummy8): Cop10[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10] =
      Cop10[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10](mapN.mapAt(f)(run))

    def map9[B](f: F[A9] => F[B]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10] =
      Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10](mapN.map9(run)(f))

    def mapAt[B](f: F[A9] => F[B])(implicit d: Dummy9): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10] =
      Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10](mapN.mapAt(f)(run))

    def map10[B](f: F[A10] => F[B]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B] =
      Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B](mapN.map10(run)(f))

    def mapAt[B](f: F[A10] => F[B])(implicit d: Dummy10): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B] =
      Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B](mapN.mapAt(f)(run))

  }

  trait Cop10LP {

    implicit def Cop10Instance[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: FFunctor[Cop10[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] with FTraverseCop[Cop10[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
      new FFunctor[Cop10[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] with FTraverseCop[Cop10[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] {
        def map[F[_], G[_]](c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10])(nt: F ~> G): Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] =
          Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](c.run.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_)))))))))))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10])(f: F ~> Lambda[a => A[G[a]]]): A[Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
          c.run match {

            case Left(x) => Functor[A].map(f(x))(y => Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Left(y)))

            case Right(Left(x)) => Functor[A].map(f(x))(y => Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Left(y))))

            case Right(Right(Left(x))) => Functor[A].map(f(x))(y => Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Left(y)))))

            case Right(Right(Right(Left(x)))) => Functor[A].map(f(x))(y => Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Left(y))))))

            case Right(Right(Right(Right(Left(x))))) => Functor[A].map(f(x))(y => Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Left(y)))))))

            case Right(Right(Right(Right(Right(Left(x)))))) => Functor[A].map(f(x))(y => Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Right(Left(y))))))))

            case Right(Right(Right(Right(Right(Right(Left(x))))))) => Functor[A].map(f(x))(y => Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Right(Right(Left(y)))))))))

            case Right(Right(Right(Right(Right(Right(Right(Left(x)))))))) => Functor[A].map(f(x))(y => Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Right(Right(Right(Left(y))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))) => Functor[A].map(f(x))(y => Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Right(Right(Right(Right(Left(y)))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(x))))))))) => Functor[A].map(f(x))(y => Cop10[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Right(Right(Right(Right(Right(y)))))))))))

          }
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A1]] =
      Inj.instance(x => Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Left(x)))

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A2]] =
      Inj.instance(x => Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Left(x))))

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A3]] =
      Inj.instance(x => Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Left(x)))))

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A4]] =
      Inj.instance(x => Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Left(x))))))

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A5]] =
      Inj.instance(x => Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Left(x)))))))

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A6]] =
      Inj.instance(x => Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Right(Left(x))))))))

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A7]] =
      Inj.instance(x => Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Right(Right(Left(x)))))))))

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A8]] =
      Inj.instance(x => Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A9]] =
      Inj.instance(x => Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))))

    implicit def inja9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A10]] =
      Inj.instance(x => Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Right(Right(Right(Right(Right(x)))))))))))

    implicit def Cop10Optional0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Optional[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A1]] =
      Optional[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A1]](c => c.run match {
        case Left(x) => Some(x)
        case _ => None
      })(x => _ => Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Left(x)))

    implicit def Cop10Optional1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Optional[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A2]] =
      Optional[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A2]](c => c.run match {
        case Right(Left(x)) => Some(x)
        case _ => None
      })(x => _ => Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Left(x))))

    implicit def Cop10Optional2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Optional[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A3]] =
      Optional[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A3]](c => c.run match {
        case Right(Right(Left(x))) => Some(x)
        case _ => None
      })(x => _ => Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Left(x)))))

    implicit def Cop10Optional3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Optional[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A4]] =
      Optional[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A4]](c => c.run match {
        case Right(Right(Right(Left(x)))) => Some(x)
        case _ => None
      })(x => _ => Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Left(x))))))

    implicit def Cop10Optional4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Optional[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A5]] =
      Optional[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A5]](c => c.run match {
        case Right(Right(Right(Right(Left(x))))) => Some(x)
        case _ => None
      })(x => _ => Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Left(x)))))))

    implicit def Cop10Optional5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Optional[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A6]] =
      Optional[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A6]](c => c.run match {
        case Right(Right(Right(Right(Right(Left(x)))))) => Some(x)
        case _ => None
      })(x => _ => Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Right(Left(x))))))))

    implicit def Cop10Optional6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Optional[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A7]] =
      Optional[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A7]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Left(x))))))) => Some(x)
        case _ => None
      })(x => _ => Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Right(Right(Left(x)))))))))

    implicit def Cop10Optional7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Optional[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A8]] =
      Optional[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A8]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Right(Left(x)))))))) => Some(x)
        case _ => None
      })(x => _ => Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))

    implicit def Cop10Optional8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Optional[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A9]] =
      Optional[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A9]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))) => Some(x)
        case _ => None
      })(x => _ => Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))))

    implicit def Cop10Optional9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Optional[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A10]] =
      Optional[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A10]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Right(Right(Right(x))))))))) => Some(x)
        case _ => None
      })(x => _ => Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](Right(Right(Right(Right(Right(Right(Right(Right(Right(x)))))))))))

  }

  object Cop10 extends Cop10LP {

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A1] =
      inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A2] =
      inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A3] =
      inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A4] =
      inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A5] =
      inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A6] =
      inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A7] =
      inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A8] =
      inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A9] =
      inja8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A10] =
      inja9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def Cop10Optional0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Optional[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A1] =
      Cop10Optional0[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def Cop10Optional1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Optional[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A2] =
      Cop10Optional1[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def Cop10Optional2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Optional[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A3] =
      Cop10Optional2[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def Cop10Optional3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Optional[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A4] =
      Cop10Optional3[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def Cop10Optional4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Optional[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A5] =
      Cop10Optional4[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def Cop10Optional5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Optional[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A6] =
      Cop10Optional5[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def Cop10Optional6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Optional[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A7] =
      Cop10Optional6[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def Cop10Optional7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Optional[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A8] =
      Cop10Optional7[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def Cop10Optional8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Optional[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A9] =
      Cop10Optional8[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def Cop10Optional9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Optional[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A10] =
      Cop10Optional9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

  }
}
