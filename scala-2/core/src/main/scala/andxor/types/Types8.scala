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

trait Types8 {
  final type Prod8[F[_], A1, A2, A3, A4, A5, A6, A7, A8] = Types8.Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]
  final val Prod8: Types8.Prod8.type = Types8.Prod8
  final type Cop8[F[_], A1, A2, A3, A4, A5, A6, A7, A8] = Types8.Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]
  final val Cop8: Types8.Cop8.type = Types8.Cop8
}

object Types8 {
  @newtype case class Prod8[F[_], A1, A2, A3, A4, A5, A6, A7, A8](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8])) { self =>
    def t1: F[A1] = run._1
    def t2: F[A2] = run._2
    def t3: F[A3] = run._3
    def t4: F[A4] = run._4
    def t5: F[A5] = run._5
    def t6: F[A6] = run._6
    def t7: F[A7] = run._7
    def t8: F[A8] = run._8

    private def mapN = new Tuple8Ops[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8]](run)

    def map1[B](f: F[A1] => F[B]): Prod8[F, B, A2, A3, A4, A5, A6, A7, A8] = {
      Prod8[F, B, A2, A3, A4, A5, A6, A7, A8](mapN.map1(f))
    }

    def map2[B](f: F[A2] => F[B]): Prod8[F, A1, B, A3, A4, A5, A6, A7, A8] = {
      Prod8[F, A1, B, A3, A4, A5, A6, A7, A8](mapN.map2(f))
    }

    def map3[B](f: F[A3] => F[B]): Prod8[F, A1, A2, B, A4, A5, A6, A7, A8] = {
      Prod8[F, A1, A2, B, A4, A5, A6, A7, A8](mapN.map3(f))
    }

    def map4[B](f: F[A4] => F[B]): Prod8[F, A1, A2, A3, B, A5, A6, A7, A8] = {
      Prod8[F, A1, A2, A3, B, A5, A6, A7, A8](mapN.map4(f))
    }

    def map5[B](f: F[A5] => F[B]): Prod8[F, A1, A2, A3, A4, B, A6, A7, A8] = {
      Prod8[F, A1, A2, A3, A4, B, A6, A7, A8](mapN.map5(f))
    }

    def map6[B](f: F[A6] => F[B]): Prod8[F, A1, A2, A3, A4, A5, B, A7, A8] = {
      Prod8[F, A1, A2, A3, A4, A5, B, A7, A8](mapN.map6(f))
    }

    def map7[B](f: F[A7] => F[B]): Prod8[F, A1, A2, A3, A4, A5, A6, B, A8] = {
      Prod8[F, A1, A2, A3, A4, A5, A6, B, A8](mapN.map7(f))
    }

    def map8[B](f: F[A8] => F[B]): Prod8[F, A1, A2, A3, A4, A5, A6, A7, B] = {
      Prod8[F, A1, A2, A3, A4, A5, A6, A7, B](mapN.map8(f))
    }

  }

  trait Prod8LP {

    implicit def Prod8Instance[A1, A2, A3, A4, A5, A6, A7, A8]: FFunctor[Prod8[*[_], A1, A2, A3, A4, A5, A6, A7, A8]] with FTraverseProd[Prod8[*[_], A1, A2, A3, A4, A5, A6, A7, A8]] =
      new FFunctor[Prod8[*[_], A1, A2, A3, A4, A5, A6, A7, A8]] with FTraverseProd[Prod8[*[_], A1, A2, A3, A4, A5, A6, A7, A8]] {
        def map[F[_], G[_]](p: Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8])(nt: F ~> G): Prod8[G, A1, A2, A3, A4, A5, A6, A7, A8] =
          Prod8[G, A1, A2, A3, A4, A5, A6, A7, A8]((nt(p.t1), nt(p.t2), nt(p.t3), nt(p.t4), nt(p.t5), nt(p.t6), nt(p.t7), nt(p.t8)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8])(f: F ~> Lambda[a => A[G[a]]]): A[Prod8[G, A1, A2, A3, A4, A5, A6, A7, A8]] =
          Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].map(f(p.t1))((i0: G[A1]) => (i1: G[A2]) => (i2: G[A3]) => (i3: G[A4]) => (i4: G[A5]) => (i5: G[A6]) => (i6: G[A7]) => (i7: G[A8]) => Prod8[G, A1, A2, A3, A4, A5, A6, A7, A8]((i0, i1, i2, i3, i4, i5, i6, i7))))(f(p.t2)))(f(p.t3)))(f(p.t4)))(f(p.t5)))(f(p.t6)))(f(p.t7)))(f(p.t8))
      }

    implicit def Prod8FoldMap[A1, A2, A3, A4, A5, A6, A7, A8]: FoldMap[Prod8[*[_], A1, A2, A3, A4, A5, A6, A7, A8], Cop8[*[_], A1, A2, A3, A4, A5, A6, A7, A8]] =
      new FoldMap[Prod8[*[_], A1, A2, A3, A4, A5, A6, A7, A8], Cop8[*[_], A1, A2, A3, A4, A5, A6, A7, A8]] {
        def emptyProd[F[_]](implicit PE: MonoidK[F]): Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8] =
          Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5], PE.empty[A6], PE.empty[A7], PE.empty[A8]))

        def unconsAll[F[_], G[_]](p: Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8])(implicit U: Uncons[F, G]): (List[Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8]], Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]) = {
          val (h1, t1) = U(p.t1)
          val (h2, t2) = U(p.t2)
          val (h3, t3) = U(p.t3)
          val (h4, t4) = U(p.t4)
          val (h5, t5) = U(p.t5)
          val (h6, t6) = U(p.t6)
          val (h7, t7) = U(p.t7)
          val (h8, t8) = U(p.t8)
          (
            List(h1.map(Inj[Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8], G[A1]].apply(_)), h2.map(Inj[Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8], G[A2]].apply(_)), h3.map(Inj[Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8], G[A3]].apply(_)), h4.map(Inj[Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8], G[A4]].apply(_)), h5.map(Inj[Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8], G[A5]].apply(_)), h6.map(Inj[Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8], G[A6]].apply(_)), h7.map(Inj[Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8], G[A7]].apply(_)), h8.map(Inj[Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8], G[A8]].apply(_))).flatten,
            Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t1, t2, t3, t4, t5, t6, t7, t8)))
        }

        def unconsOne[F[_], G[_]](p: Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], c: Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8])(implicit U: Uncons[F, G]): (Option[Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8]], Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]) =
          c.run match {

            case Left(_) =>
              val (h, t) = U(p.t1)
              (h.map(v => Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8](Left(v))), Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8)))

            case Right(Left(_)) =>
              val (h, t) = U(p.t2)
              (h.map(v => Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8](Right(Left(v)))), Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((p.t1, t, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8)))

            case Right(Right(Left(_))) =>
              val (h, t) = U(p.t3)
              (h.map(v => Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Left(v))))), Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((p.t1, p.t2, t, p.t4, p.t5, p.t6, p.t7, p.t8)))

            case Right(Right(Right(Left(_)))) =>
              val (h, t) = U(p.t4)
              (h.map(v => Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Left(v)))))), Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((p.t1, p.t2, p.t3, t, p.t5, p.t6, p.t7, p.t8)))

            case Right(Right(Right(Right(Left(_))))) =>
              val (h, t) = U(p.t5)
              (h.map(v => Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Right(Left(v))))))), Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((p.t1, p.t2, p.t3, p.t4, t, p.t6, p.t7, p.t8)))

            case Right(Right(Right(Right(Right(Left(_)))))) =>
              val (h, t) = U(p.t6)
              (h.map(v => Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Right(Right(Left(v)))))))), Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((p.t1, p.t2, p.t3, p.t4, p.t5, t, p.t7, p.t8)))

            case Right(Right(Right(Right(Right(Right(Left(_))))))) =>
              val (h, t) = U(p.t7)
              (h.map(v => Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Right(Right(Right(Left(v))))))))), Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, t, p.t8)))

            case Right(Right(Right(Right(Right(Right(Right(_))))))) =>
              val (h, t) = U(p.t8)
              (h.map(v => Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Right(Right(Right(Right(v))))))))), Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, t)))

          }
      }

    def Prod8TupleIso[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Iso[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8])] =
      Iso((_: Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]).run)(Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8](_: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8])))

    implicit def Prod8Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8])]): Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = {
      val iso = Prod8TupleIso[F, A1, A2, A3, A4, A5, A6, A7, A8]
      M.imap(iso.reverseGet)(iso.get)
    }

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A1]] = {
      val t = M.empty
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((x, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8)))
    }

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A2]] = {
      val t = M.empty
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t.t1, x, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8)))
    }

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A3]] = {
      val t = M.empty
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t.t1, t.t2, x, t.t4, t.t5, t.t6, t.t7, t.t8)))
    }

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A4]] = {
      val t = M.empty
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t.t1, t.t2, t.t3, x, t.t5, t.t6, t.t7, t.t8)))
    }

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A5]] = {
      val t = M.empty
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t.t1, t.t2, t.t3, t.t4, x, t.t6, t.t7, t.t8)))
    }

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A6]] = {
      val t = M.empty
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t.t1, t.t2, t.t3, t.t4, t.t5, x, t.t7, t.t8)))
    }

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A7]] = {
      val t = M.empty
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, x, t.t8)))
    }

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A8]] = {
      val t = M.empty
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, x)))
    }

    implicit def injProdToVecCop[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Vector[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]], Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] =
      Inj.instance(p => Vector(
        Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Left(p.t1)),
        Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Right(Left(p.t2))),
        Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Left(p.t3)))),
        Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Left(p.t4))))),
        Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Right(Left(p.t5)))))),
        Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Right(Right(Left(p.t6))))))),
        Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Right(Right(Right(Left(p.t7)))))))),
        Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Right(Right(Right(Right(p.t8))))))))))

    implicit def Prod8Lens0[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Lens[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A1]] =
      Lens[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A1]](p => p.t1)(x => p =>
        Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((x, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8)))

    implicit def Prod8Lens1[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Lens[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A2]] =
      Lens[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A2]](p => p.t2)(x => p =>
        Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((p.t1, x, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8)))

    implicit def Prod8Lens2[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Lens[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A3]] =
      Lens[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A3]](p => p.t3)(x => p =>
        Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((p.t1, p.t2, x, p.t4, p.t5, p.t6, p.t7, p.t8)))

    implicit def Prod8Lens3[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Lens[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A4]] =
      Lens[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A4]](p => p.t4)(x => p =>
        Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((p.t1, p.t2, p.t3, x, p.t5, p.t6, p.t7, p.t8)))

    implicit def Prod8Lens4[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Lens[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A5]] =
      Lens[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A5]](p => p.t5)(x => p =>
        Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((p.t1, p.t2, p.t3, p.t4, x, p.t6, p.t7, p.t8)))

    implicit def Prod8Lens5[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Lens[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A6]] =
      Lens[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A6]](p => p.t6)(x => p =>
        Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((p.t1, p.t2, p.t3, p.t4, p.t5, x, p.t7, p.t8)))

    implicit def Prod8Lens6[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Lens[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A7]] =
      Lens[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A7]](p => p.t7)(x => p =>
        Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, x, p.t8)))

    implicit def Prod8Lens7[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Lens[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A8]] =
      Lens[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A8]](p => p.t8)(x => p =>
        Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, x)))

  }

  object Prod8 extends Prod8LP {

    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A1] =
      lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A2] =
      lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A3] =
      lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A4] =
      lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A5] =
      lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A6] =
      lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A7] =
      lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A8] =
      lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def Prod8Lens0Id[A1, A2, A3, A4, A5, A6, A7, A8]: Lens[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A1] =
      Prod8Lens0[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def Prod8Lens1Id[A1, A2, A3, A4, A5, A6, A7, A8]: Lens[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A2] =
      Prod8Lens1[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def Prod8Lens2Id[A1, A2, A3, A4, A5, A6, A7, A8]: Lens[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A3] =
      Prod8Lens2[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def Prod8Lens3Id[A1, A2, A3, A4, A5, A6, A7, A8]: Lens[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A4] =
      Prod8Lens3[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def Prod8Lens4Id[A1, A2, A3, A4, A5, A6, A7, A8]: Lens[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A5] =
      Prod8Lens4[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def Prod8Lens5Id[A1, A2, A3, A4, A5, A6, A7, A8]: Lens[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A6] =
      Prod8Lens5[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def Prod8Lens6Id[A1, A2, A3, A4, A5, A6, A7, A8]: Lens[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A7] =
      Prod8Lens6[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def Prod8Lens7Id[A1, A2, A3, A4, A5, A6, A7, A8]: Lens[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A8] =
      Prod8Lens7[Id, A1, A2, A3, A4, A5, A6, A7, A8]

  }

  @newtype case class Cop8[F[_], A1, A2, A3, A4, A5, A6, A7, A8](run: Either[F[A1], Either[F[A2], Either[F[A3], Either[F[A4], Either[F[A5], Either[F[A6], Either[F[A7], F[A8]]]]]]]]) {
    private def mapN = new Either8Ops[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8]](run)

    def map1[B](f: F[A1] => F[B]): Cop8[F, B, A2, A3, A4, A5, A6, A7, A8] =
      Cop8[F, B, A2, A3, A4, A5, A6, A7, A8](mapN.map1(f))

    def map2[B](f: F[A2] => F[B]): Cop8[F, A1, B, A3, A4, A5, A6, A7, A8] =
      Cop8[F, A1, B, A3, A4, A5, A6, A7, A8](mapN.map2(f))

    def map3[B](f: F[A3] => F[B]): Cop8[F, A1, A2, B, A4, A5, A6, A7, A8] =
      Cop8[F, A1, A2, B, A4, A5, A6, A7, A8](mapN.map3(f))

    def map4[B](f: F[A4] => F[B]): Cop8[F, A1, A2, A3, B, A5, A6, A7, A8] =
      Cop8[F, A1, A2, A3, B, A5, A6, A7, A8](mapN.map4(f))

    def map5[B](f: F[A5] => F[B]): Cop8[F, A1, A2, A3, A4, B, A6, A7, A8] =
      Cop8[F, A1, A2, A3, A4, B, A6, A7, A8](mapN.map5(f))

    def map6[B](f: F[A6] => F[B]): Cop8[F, A1, A2, A3, A4, A5, B, A7, A8] =
      Cop8[F, A1, A2, A3, A4, A5, B, A7, A8](mapN.map6(f))

    def map7[B](f: F[A7] => F[B]): Cop8[F, A1, A2, A3, A4, A5, A6, B, A8] =
      Cop8[F, A1, A2, A3, A4, A5, A6, B, A8](mapN.map7(f))

    def map8[B](f: F[A8] => F[B]): Cop8[F, A1, A2, A3, A4, A5, A6, A7, B] =
      Cop8[F, A1, A2, A3, A4, A5, A6, A7, B](mapN.map8(f))

  }

  trait Cop8LP {

    implicit def Cop8Instance[A1, A2, A3, A4, A5, A6, A7, A8]: FFunctor[Cop8[*[_], A1, A2, A3, A4, A5, A6, A7, A8]] with FTraverseCop[Cop8[*[_], A1, A2, A3, A4, A5, A6, A7, A8]] =
      new FFunctor[Cop8[*[_], A1, A2, A3, A4, A5, A6, A7, A8]] with FTraverseCop[Cop8[*[_], A1, A2, A3, A4, A5, A6, A7, A8]] {
        def map[F[_], G[_]](c: Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8])(nt: F ~> G): Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8] =
          Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8](c.run.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_)))))))))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8])(f: F ~> Lambda[a => A[G[a]]]): A[Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8]] =
          c.run match {

            case Left(x) => Functor[A].map(f(x))(y => Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8](Left(y)))

            case Right(Left(x)) => Functor[A].map(f(x))(y => Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8](Right(Left(y))))

            case Right(Right(Left(x))) => Functor[A].map(f(x))(y => Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Left(y)))))

            case Right(Right(Right(Left(x)))) => Functor[A].map(f(x))(y => Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Left(y))))))

            case Right(Right(Right(Right(Left(x))))) => Functor[A].map(f(x))(y => Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Right(Left(y)))))))

            case Right(Right(Right(Right(Right(Left(x)))))) => Functor[A].map(f(x))(y => Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Right(Right(Left(y))))))))

            case Right(Right(Right(Right(Right(Right(Left(x))))))) => Functor[A].map(f(x))(y => Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Right(Right(Right(Left(y)))))))))

            case Right(Right(Right(Right(Right(Right(Right(x))))))) => Functor[A].map(f(x))(y => Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Right(Right(Right(Right(y)))))))))

          }
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A1]] =
      Inj.instance(x => Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Left(x)))

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A2]] =
      Inj.instance(x => Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Right(Left(x))))

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A3]] =
      Inj.instance(x => Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Left(x)))))

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A4]] =
      Inj.instance(x => Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Left(x))))))

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A5]] =
      Inj.instance(x => Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Right(Left(x)))))))

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A6]] =
      Inj.instance(x => Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Right(Right(Left(x))))))))

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A7]] =
      Inj.instance(x => Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Right(Right(Right(Left(x)))))))))

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A8]] =
      Inj.instance(x => Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Right(Right(Right(Right(x)))))))))

    implicit def injCopToProd[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]] =
      Inj.instance(_.run match {
        case Left(x) => Prod8.lifta0F[F, A1, A2, A3, A4, A5, A6, A7, A8].apply(x)
        case Right(Left(x)) => Prod8.lifta1F[F, A1, A2, A3, A4, A5, A6, A7, A8].apply(x)
        case Right(Right(Left(x))) => Prod8.lifta2F[F, A1, A2, A3, A4, A5, A6, A7, A8].apply(x)
        case Right(Right(Right(Left(x)))) => Prod8.lifta3F[F, A1, A2, A3, A4, A5, A6, A7, A8].apply(x)
        case Right(Right(Right(Right(Left(x))))) => Prod8.lifta4F[F, A1, A2, A3, A4, A5, A6, A7, A8].apply(x)
        case Right(Right(Right(Right(Right(Left(x)))))) => Prod8.lifta5F[F, A1, A2, A3, A4, A5, A6, A7, A8].apply(x)
        case Right(Right(Right(Right(Right(Right(Left(x))))))) => Prod8.lifta6F[F, A1, A2, A3, A4, A5, A6, A7, A8].apply(x)
        case Right(Right(Right(Right(Right(Right(Right(x))))))) => Prod8.lifta7F[F, A1, A2, A3, A4, A5, A6, A7, A8].apply(x)
      })

    implicit def Cop8Optional0[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Optional[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A1]] =
      Optional[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A1]](c => c.run match {
        case Left(x) => Some(x)
        case _ => None
      })(x => _ => Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Left(x)))

    implicit def Cop8Optional1[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Optional[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A2]] =
      Optional[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A2]](c => c.run match {
        case Right(Left(x)) => Some(x)
        case _ => None
      })(x => _ => Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Right(Left(x))))

    implicit def Cop8Optional2[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Optional[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A3]] =
      Optional[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A3]](c => c.run match {
        case Right(Right(Left(x))) => Some(x)
        case _ => None
      })(x => _ => Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Left(x)))))

    implicit def Cop8Optional3[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Optional[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A4]] =
      Optional[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A4]](c => c.run match {
        case Right(Right(Right(Left(x)))) => Some(x)
        case _ => None
      })(x => _ => Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Left(x))))))

    implicit def Cop8Optional4[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Optional[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A5]] =
      Optional[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A5]](c => c.run match {
        case Right(Right(Right(Right(Left(x))))) => Some(x)
        case _ => None
      })(x => _ => Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Right(Left(x)))))))

    implicit def Cop8Optional5[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Optional[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A6]] =
      Optional[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A6]](c => c.run match {
        case Right(Right(Right(Right(Right(Left(x)))))) => Some(x)
        case _ => None
      })(x => _ => Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Right(Right(Left(x))))))))

    implicit def Cop8Optional6[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Optional[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A7]] =
      Optional[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A7]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Left(x))))))) => Some(x)
        case _ => None
      })(x => _ => Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Right(Right(Right(Left(x)))))))))

    implicit def Cop8Optional7[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Optional[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A8]] =
      Optional[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A8]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Right(x))))))) => Some(x)
        case _ => None
      })(x => _ => Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](Right(Right(Right(Right(Right(Right(Right(x)))))))))

  }

  object Cop8 extends Cop8LP {

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A1] =
      inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A2] =
      inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A3] =
      inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A4] =
      inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A5] =
      inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A6] =
      inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A7] =
      inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A8] =
      inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def Cop8Optional0Id[A1, A2, A3, A4, A5, A6, A7, A8]: Optional[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A1] =
      Cop8Optional0[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def Cop8Optional1Id[A1, A2, A3, A4, A5, A6, A7, A8]: Optional[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A2] =
      Cop8Optional1[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def Cop8Optional2Id[A1, A2, A3, A4, A5, A6, A7, A8]: Optional[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A3] =
      Cop8Optional2[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def Cop8Optional3Id[A1, A2, A3, A4, A5, A6, A7, A8]: Optional[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A4] =
      Cop8Optional3[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def Cop8Optional4Id[A1, A2, A3, A4, A5, A6, A7, A8]: Optional[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A5] =
      Cop8Optional4[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def Cop8Optional5Id[A1, A2, A3, A4, A5, A6, A7, A8]: Optional[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A6] =
      Cop8Optional5[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def Cop8Optional6Id[A1, A2, A3, A4, A5, A6, A7, A8]: Optional[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A7] =
      Cop8Optional6[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def Cop8Optional7Id[A1, A2, A3, A4, A5, A6, A7, A8]: Optional[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A8] =
      Cop8Optional7[Id, A1, A2, A3, A4, A5, A6, A7, A8]

  }
}
