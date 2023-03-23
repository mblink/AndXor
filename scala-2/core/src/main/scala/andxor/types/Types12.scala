package andxor.types

import andxor._
import andxor.either._
import andxor.tuple._
import monocle.{Lens, Optional}
import cats.{~>, Applicative, Functor, Id, Monoid, MonoidK}
import cats.syntax.either._
import cats.syntax.invariant._
import io.estatico.newtype.macros.newtype
import monocle.Iso

trait Types12 {
  final type Prod12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Types12.Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]
  final val Prod12: Types12.Prod12.type = Types12.Prod12
  final type Cop12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Types12.Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]
  final val Cop12: Types12.Cop12.type = Types12.Cop12
}

object Types12 {
  @newtype case class Prod12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12])) { self =>
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

    def map1[B](f: F[A1] => F[B]): Prod12[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = {
      Prod12[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](run.map1(f))
    }

    def map2[B](f: F[A2] => F[B]): Prod12[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = {
      Prod12[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](run.map2(f))
    }

    def map3[B](f: F[A3] => F[B]): Prod12[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12] = {
      Prod12[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12](run.map3(f))
    }

    def map4[B](f: F[A4] => F[B]): Prod12[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12] = {
      Prod12[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12](run.map4(f))
    }

    def map5[B](f: F[A5] => F[B]): Prod12[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12] = {
      Prod12[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12](run.map5(f))
    }

    def map6[B](f: F[A6] => F[B]): Prod12[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12] = {
      Prod12[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12](run.map6(f))
    }

    def map7[B](f: F[A7] => F[B]): Prod12[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12] = {
      Prod12[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12](run.map7(f))
    }

    def map8[B](f: F[A8] => F[B]): Prod12[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12] = {
      Prod12[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12](run.map8(f))
    }

    def map9[B](f: F[A9] => F[B]): Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12] = {
      Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12](run.map9(f))
    }

    def map10[B](f: F[A10] => F[B]): Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12] = {
      Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12](run.map10(f))
    }

    def map11[B](f: F[A11] => F[B]): Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12] = {
      Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12](run.map11(f))
    }

    def map12[B](f: F[A12] => F[B]): Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B] = {
      Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B](run.map12(f))
    }

  }

  trait Prod12LP {

    implicit def Prod12Instance[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: FFunctor[Prod12[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] with FTraverseProd[Prod12[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      new FFunctor[Prod12[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] with FTraverseProd[Prod12[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] {
        def map[F[_], G[_]](p: Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12])(nt: F ~> G): Prod12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] =
          Prod12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((nt(p.t1), nt(p.t2), nt(p.t3), nt(p.t4), nt(p.t5), nt(p.t6), nt(p.t7), nt(p.t8), nt(p.t9), nt(p.t10), nt(p.t11), nt(p.t12)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12])(f: F ~> Lambda[a => A[G[a]]]): A[Prod12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
          Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].map(f(p.t1))((i0: G[A1]) => (i1: G[A2]) => (i2: G[A3]) => (i3: G[A4]) => (i4: G[A5]) => (i5: G[A6]) => (i6: G[A7]) => (i7: G[A8]) => (i8: G[A9]) => (i9: G[A10]) => (i10: G[A11]) => (i11: G[A12]) => Prod12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11))))(f(p.t2)))(f(p.t3)))(f(p.t4)))(f(p.t5)))(f(p.t6)))(f(p.t7)))(f(p.t8)))(f(p.t9)))(f(p.t10)))(f(p.t11)))(f(p.t12))
      }

    implicit def Prod12FoldMap[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: FoldMap[Prod12[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], Cop12[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      new FoldMap[Prod12[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], Cop12[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] {
        def emptyProd[F[_]](implicit PE: MonoidK[F]): Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] =
          Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5], PE.empty[A6], PE.empty[A7], PE.empty[A8], PE.empty[A9], PE.empty[A10], PE.empty[A11], PE.empty[A12]))

        def unconsAll[F[_], G[_]](p: Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12])(implicit U: Uncons[F, G]): (List[Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]) = {
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
          (
            List(h1.map(Inj[Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], G[A1]].apply(_)), h2.map(Inj[Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], G[A2]].apply(_)), h3.map(Inj[Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], G[A3]].apply(_)), h4.map(Inj[Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], G[A4]].apply(_)), h5.map(Inj[Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], G[A5]].apply(_)), h6.map(Inj[Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], G[A6]].apply(_)), h7.map(Inj[Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], G[A7]].apply(_)), h8.map(Inj[Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], G[A8]].apply(_)), h9.map(Inj[Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], G[A9]].apply(_)), h10.map(Inj[Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], G[A10]].apply(_)), h11.map(Inj[Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], G[A11]].apply(_)), h12.map(Inj[Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], G[A12]].apply(_))).flatten,
            Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)))
        }

        def unconsOne[F[_], G[_]](p: Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], c: Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12])(implicit U: Uncons[F, G]): (Option[Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]) =
          c.run match {

            case Left(_) =>
              val (h, t) = U(p.t1)
              (h.map(v => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Left(v))), Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12)))

            case Right(Left(_)) =>
              val (h, t) = U(p.t2)
              (h.map(v => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Left(v)))), Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, t, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12)))

            case Right(Right(Left(_))) =>
              val (h, t) = U(p.t3)
              (h.map(v => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Left(v))))), Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, p.t2, t, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12)))

            case Right(Right(Right(Left(_)))) =>
              val (h, t) = U(p.t4)
              (h.map(v => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Left(v)))))), Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, p.t2, p.t3, t, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12)))

            case Right(Right(Right(Right(Left(_))))) =>
              val (h, t) = U(p.t5)
              (h.map(v => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Left(v))))))), Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, p.t2, p.t3, p.t4, t, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12)))

            case Right(Right(Right(Right(Right(Left(_)))))) =>
              val (h, t) = U(p.t6)
              (h.map(v => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Left(v)))))))), Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, p.t2, p.t3, p.t4, p.t5, t, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12)))

            case Right(Right(Right(Right(Right(Right(Left(_))))))) =>
              val (h, t) = U(p.t7)
              (h.map(v => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Left(v))))))))), Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, t, p.t8, p.t9, p.t10, p.t11, p.t12)))

            case Right(Right(Right(Right(Right(Right(Right(Left(_)))))))) =>
              val (h, t) = U(p.t8)
              (h.map(v => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Left(v)))))))))), Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, t, p.t9, p.t10, p.t11, p.t12)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Left(_))))))))) =>
              val (h, t) = U(p.t9)
              (h.map(v => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Right(Left(v))))))))))), Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, t, p.t10, p.t11, p.t12)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(_)))))))))) =>
              val (h, t) = U(p.t10)
              (h.map(v => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(v)))))))))))), Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, t, p.t11, p.t12)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(_))))))))))) =>
              val (h, t) = U(p.t11)
              (h.map(v => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(v))))))))))))), Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, t, p.t12)))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(_))))))))))) =>
              val (h, t) = U(p.t12)
              (h.map(v => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(v))))))))))))), Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, t)))

          }
      }

    def Prod12TupleIso[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Iso[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12])] =
      Iso((_: Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]).run)(Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](_: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12])))

    implicit def Prod12Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12])]): Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = {
      val iso = Prod12TupleIso[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]
      M.imap(iso.reverseGet)(iso.get)
    }

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A1]] = {
      val t = M.empty
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((x, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12)))
    }

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A2]] = {
      val t = M.empty
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t.t1, x, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12)))
    }

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A3]] = {
      val t = M.empty
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t.t1, t.t2, x, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12)))
    }

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A4]] = {
      val t = M.empty
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t.t1, t.t2, t.t3, x, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12)))
    }

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A5]] = {
      val t = M.empty
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t.t1, t.t2, t.t3, t.t4, x, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12)))
    }

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A6]] = {
      val t = M.empty
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t.t1, t.t2, t.t3, t.t4, t.t5, x, t.t7, t.t8, t.t9, t.t10, t.t11, t.t12)))
    }

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A7]] = {
      val t = M.empty
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, x, t.t8, t.t9, t.t10, t.t11, t.t12)))
    }

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A8]] = {
      val t = M.empty
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, x, t.t9, t.t10, t.t11, t.t12)))
    }

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A9]] = {
      val t = M.empty
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, x, t.t10, t.t11, t.t12)))
    }

    implicit def lifta9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A10]] = {
      val t = M.empty
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, x, t.t11, t.t12)))
    }

    implicit def lifta10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A11]] = {
      val t = M.empty
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, x, t.t12)))
    }

    implicit def lifta11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A12]] = {
      val t = M.empty
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9, t.t10, t.t11, x)))
    }

    implicit def injProdToVecCop[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Vector[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      Inj.instance(p => Vector(
        Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Left(p.t1)),
        Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Left(p.t2))),
        Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Left(p.t3)))),
        Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Left(p.t4))))),
        Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Left(p.t5)))))),
        Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Left(p.t6))))))),
        Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Left(p.t7)))))))),
        Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Left(p.t8))))))))),
        Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Right(Left(p.t9)))))))))),
        Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(p.t10))))))))))),
        Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(p.t11)))))))))))),
        Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(p.t12))))))))))))))

    implicit def Prod12Lens0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A1]] =
      Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A1]](p => p.t1)(x => p =>
        Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((x, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12)))

    implicit def Prod12Lens1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A2]] =
      Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A2]](p => p.t2)(x => p =>
        Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, x, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12)))

    implicit def Prod12Lens2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A3]] =
      Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A3]](p => p.t3)(x => p =>
        Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, p.t2, x, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12)))

    implicit def Prod12Lens3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A4]] =
      Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A4]](p => p.t4)(x => p =>
        Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, p.t2, p.t3, x, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12)))

    implicit def Prod12Lens4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A5]] =
      Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A5]](p => p.t5)(x => p =>
        Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, p.t2, p.t3, p.t4, x, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12)))

    implicit def Prod12Lens5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A6]] =
      Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A6]](p => p.t6)(x => p =>
        Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, p.t2, p.t3, p.t4, p.t5, x, p.t7, p.t8, p.t9, p.t10, p.t11, p.t12)))

    implicit def Prod12Lens6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A7]] =
      Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A7]](p => p.t7)(x => p =>
        Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, x, p.t8, p.t9, p.t10, p.t11, p.t12)))

    implicit def Prod12Lens7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A8]] =
      Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A8]](p => p.t8)(x => p =>
        Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, x, p.t9, p.t10, p.t11, p.t12)))

    implicit def Prod12Lens8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A9]] =
      Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A9]](p => p.t9)(x => p =>
        Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, x, p.t10, p.t11, p.t12)))

    implicit def Prod12Lens9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A10]] =
      Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A10]](p => p.t10)(x => p =>
        Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, x, p.t11, p.t12)))

    implicit def Prod12Lens10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A11]] =
      Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A11]](p => p.t11)(x => p =>
        Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, x, p.t12)))

    implicit def Prod12Lens11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A12]] =
      Lens[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A12]](p => p.t12)(x => p =>
        Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9, p.t10, p.t11, x)))

  }

  object Prod12 extends Prod12LP {

    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A1] =
      lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A2] =
      lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A3] =
      lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A4] =
      lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A5] =
      lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A6] =
      lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A7] =
      lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A8] =
      lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A9] =
      lifta8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A10] =
      lifta9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A11] =
      lifta10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A12] =
      lifta11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Prod12Lens0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A1] =
      Prod12Lens0[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Prod12Lens1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A2] =
      Prod12Lens1[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Prod12Lens2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A3] =
      Prod12Lens2[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Prod12Lens3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A4] =
      Prod12Lens3[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Prod12Lens4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A5] =
      Prod12Lens4[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Prod12Lens5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A6] =
      Prod12Lens5[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Prod12Lens6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A7] =
      Prod12Lens6[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Prod12Lens7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A8] =
      Prod12Lens7[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Prod12Lens8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A9] =
      Prod12Lens8[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Prod12Lens9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A10] =
      Prod12Lens9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Prod12Lens10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A11] =
      Prod12Lens10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Prod12Lens11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Lens[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A12] =
      Prod12Lens11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

  }

  @newtype case class Cop12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](run: Either[F[A1], Either[F[A2], Either[F[A3], Either[F[A4], Either[F[A5], Either[F[A6], Either[F[A7], Either[F[A8], Either[F[A9], Either[F[A10], Either[F[A11], F[A12]]]]]]]]]]]]) {

    def map1[B](f: F[A1] => F[B]): Cop12[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] =
      Cop12[F, B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](run.map1(f))

    def map2[B](f: F[A2] => F[B]): Cop12[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] =
      Cop12[F, A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](run.map2(f))

    def map3[B](f: F[A3] => F[B]): Cop12[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12] =
      Cop12[F, A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12](run.map3(f))

    def map4[B](f: F[A4] => F[B]): Cop12[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12] =
      Cop12[F, A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12](run.map4(f))

    def map5[B](f: F[A5] => F[B]): Cop12[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12] =
      Cop12[F, A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12](run.map5(f))

    def map6[B](f: F[A6] => F[B]): Cop12[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12] =
      Cop12[F, A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12](run.map6(f))

    def map7[B](f: F[A7] => F[B]): Cop12[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12] =
      Cop12[F, A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12](run.map7(f))

    def map8[B](f: F[A8] => F[B]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12] =
      Cop12[F, A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12](run.map8(f))

    def map9[B](f: F[A9] => F[B]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12] =
      Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12](run.map9(f))

    def map10[B](f: F[A10] => F[B]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12] =
      Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12](run.map10(f))

    def map11[B](f: F[A11] => F[B]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12] =
      Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12](run.map11(f))

    def map12[B](f: F[A12] => F[B]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B] =
      Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B](run.map12(f))

  }

  trait Cop12LP {

    implicit def Cop12Instance[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: FFunctor[Cop12[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] with FTraverseCop[Cop12[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      new FFunctor[Cop12[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] with FTraverseCop[Cop12[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] {
        def map[F[_], G[_]](c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12])(nt: F ~> G): Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] =
          Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](c.run.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_)))))))))))))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12])(f: F ~> Lambda[a => A[G[a]]]): A[Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
          c.run match {

            case Left(x) => Functor[A].map(f(x))(y => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Left(y)))

            case Right(Left(x)) => Functor[A].map(f(x))(y => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Left(y))))

            case Right(Right(Left(x))) => Functor[A].map(f(x))(y => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Left(y)))))

            case Right(Right(Right(Left(x)))) => Functor[A].map(f(x))(y => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Left(y))))))

            case Right(Right(Right(Right(Left(x))))) => Functor[A].map(f(x))(y => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Left(y)))))))

            case Right(Right(Right(Right(Right(Left(x)))))) => Functor[A].map(f(x))(y => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Left(y))))))))

            case Right(Right(Right(Right(Right(Right(Left(x))))))) => Functor[A].map(f(x))(y => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Left(y)))))))))

            case Right(Right(Right(Right(Right(Right(Right(Left(x)))))))) => Functor[A].map(f(x))(y => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Left(y))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))) => Functor[A].map(f(x))(y => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Right(Left(y)))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))) => Functor[A].map(f(x))(y => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(y))))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))) => Functor[A].map(f(x))(y => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(y)))))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(x))))))))))) => Functor[A].map(f(x))(y => Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(y)))))))))))))

          }
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A1]] =
      Inj.instance(x => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Left(x)))

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A2]] =
      Inj.instance(x => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Left(x))))

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A3]] =
      Inj.instance(x => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Left(x)))))

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A4]] =
      Inj.instance(x => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Left(x))))))

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A5]] =
      Inj.instance(x => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Left(x)))))))

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A6]] =
      Inj.instance(x => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Left(x))))))))

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A7]] =
      Inj.instance(x => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Left(x)))))))))

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A8]] =
      Inj.instance(x => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A9]] =
      Inj.instance(x => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))))

    implicit def inja9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A10]] =
      Inj.instance(x => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))))

    implicit def inja10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A11]] =
      Inj.instance(x => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))))))

    implicit def inja11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A12]] =
      Inj.instance(x => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(x)))))))))))))

    implicit def injCopToProd[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      Inj.instance(_.run match {
        case Left(x) => Prod12.lifta0F[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].apply(x)
        case Right(Left(x)) => Prod12.lifta1F[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].apply(x)
        case Right(Right(Left(x))) => Prod12.lifta2F[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].apply(x)
        case Right(Right(Right(Left(x)))) => Prod12.lifta3F[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].apply(x)
        case Right(Right(Right(Right(Left(x))))) => Prod12.lifta4F[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].apply(x)
        case Right(Right(Right(Right(Right(Left(x)))))) => Prod12.lifta5F[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].apply(x)
        case Right(Right(Right(Right(Right(Right(Left(x))))))) => Prod12.lifta6F[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].apply(x)
        case Right(Right(Right(Right(Right(Right(Right(Left(x)))))))) => Prod12.lifta7F[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].apply(x)
        case Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))) => Prod12.lifta8F[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].apply(x)
        case Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))) => Prod12.lifta9F[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].apply(x)
        case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))) => Prod12.lifta10F[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].apply(x)
        case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(x))))))))))) => Prod12.lifta11F[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].apply(x)
      })

    implicit def Cop12Optional0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A1]] =
      Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A1]](c => c.run match {
        case Left(x) => Some(x)
        case _ => None
      })(x => _ => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Left(x)))

    implicit def Cop12Optional1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A2]] =
      Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A2]](c => c.run match {
        case Right(Left(x)) => Some(x)
        case _ => None
      })(x => _ => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Left(x))))

    implicit def Cop12Optional2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A3]] =
      Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A3]](c => c.run match {
        case Right(Right(Left(x))) => Some(x)
        case _ => None
      })(x => _ => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Left(x)))))

    implicit def Cop12Optional3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A4]] =
      Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A4]](c => c.run match {
        case Right(Right(Right(Left(x)))) => Some(x)
        case _ => None
      })(x => _ => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Left(x))))))

    implicit def Cop12Optional4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A5]] =
      Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A5]](c => c.run match {
        case Right(Right(Right(Right(Left(x))))) => Some(x)
        case _ => None
      })(x => _ => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Left(x)))))))

    implicit def Cop12Optional5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A6]] =
      Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A6]](c => c.run match {
        case Right(Right(Right(Right(Right(Left(x)))))) => Some(x)
        case _ => None
      })(x => _ => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Left(x))))))))

    implicit def Cop12Optional6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A7]] =
      Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A7]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Left(x))))))) => Some(x)
        case _ => None
      })(x => _ => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Left(x)))))))))

    implicit def Cop12Optional7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A8]] =
      Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A8]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Right(Left(x)))))))) => Some(x)
        case _ => None
      })(x => _ => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))

    implicit def Cop12Optional8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A9]] =
      Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A9]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))) => Some(x)
        case _ => None
      })(x => _ => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))))

    implicit def Cop12Optional9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A10]] =
      Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A10]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))) => Some(x)
        case _ => None
      })(x => _ => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))))

    implicit def Cop12Optional10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A11]] =
      Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A11]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))) => Some(x)
        case _ => None
      })(x => _ => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(x)))))))))))))

    implicit def Cop12Optional11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A12]] =
      Optional[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A12]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(x))))))))))) => Some(x)
        case _ => None
      })(x => _ => Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(x)))))))))))))

  }

  object Cop12 extends Cop12LP {

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A1] =
      inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A2] =
      inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A3] =
      inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A4] =
      inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A5] =
      inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A6] =
      inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A7] =
      inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A8] =
      inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A9] =
      inja8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A10] =
      inja9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A11] =
      inja10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A12] =
      inja11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Cop12Optional0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A1] =
      Cop12Optional0[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Cop12Optional1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A2] =
      Cop12Optional1[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Cop12Optional2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A3] =
      Cop12Optional2[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Cop12Optional3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A4] =
      Cop12Optional3[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Cop12Optional4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A5] =
      Cop12Optional4[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Cop12Optional5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A6] =
      Cop12Optional5[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Cop12Optional6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A7] =
      Cop12Optional6[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Cop12Optional7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A8] =
      Cop12Optional7[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Cop12Optional8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A9] =
      Cop12Optional8[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Cop12Optional9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A10] =
      Cop12Optional9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Cop12Optional10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A11] =
      Cop12Optional10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def Cop12Optional11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Optional[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A12] =
      Cop12Optional11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

  }
}
