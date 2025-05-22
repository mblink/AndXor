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

trait Types9 {
  final type Prod9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9] = Types9.Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]
  final val Prod9: Types9.Prod9.type = Types9.Prod9
  final type Cop9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9] = Types9.Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]
  final val Cop9: Types9.Cop9.type = Types9.Cop9
}

object Types9 {
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

    private def mapN = new Tuple9Ops[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9]](run)

    def map1[B](f: F[A1] => F[B]): Prod9[F, B, A2, A3, A4, A5, A6, A7, A8, A9] = {
      Prod9[F, B, A2, A3, A4, A5, A6, A7, A8, A9](mapN.map1(f))
    }

    def map2[B](f: F[A2] => F[B]): Prod9[F, A1, B, A3, A4, A5, A6, A7, A8, A9] = {
      Prod9[F, A1, B, A3, A4, A5, A6, A7, A8, A9](mapN.map2(f))
    }

    def map3[B](f: F[A3] => F[B]): Prod9[F, A1, A2, B, A4, A5, A6, A7, A8, A9] = {
      Prod9[F, A1, A2, B, A4, A5, A6, A7, A8, A9](mapN.map3(f))
    }

    def map4[B](f: F[A4] => F[B]): Prod9[F, A1, A2, A3, B, A5, A6, A7, A8, A9] = {
      Prod9[F, A1, A2, A3, B, A5, A6, A7, A8, A9](mapN.map4(f))
    }

    def map5[B](f: F[A5] => F[B]): Prod9[F, A1, A2, A3, A4, B, A6, A7, A8, A9] = {
      Prod9[F, A1, A2, A3, A4, B, A6, A7, A8, A9](mapN.map5(f))
    }

    def map6[B](f: F[A6] => F[B]): Prod9[F, A1, A2, A3, A4, A5, B, A7, A8, A9] = {
      Prod9[F, A1, A2, A3, A4, A5, B, A7, A8, A9](mapN.map6(f))
    }

    def map7[B](f: F[A7] => F[B]): Prod9[F, A1, A2, A3, A4, A5, A6, B, A8, A9] = {
      Prod9[F, A1, A2, A3, A4, A5, A6, B, A8, A9](mapN.map7(f))
    }

    def map8[B](f: F[A8] => F[B]): Prod9[F, A1, A2, A3, A4, A5, A6, A7, B, A9] = {
      Prod9[F, A1, A2, A3, A4, A5, A6, A7, B, A9](mapN.map8(f))
    }

    def map9[B](f: F[A9] => F[B]): Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, B] = {
      Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, B](mapN.map9(f))
    }

  }

  trait Prod9LP {

    implicit def Prod9Instance[A1, A2, A3, A4, A5, A6, A7, A8, A9]: FFunctor[Prod9[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]] with FTraverseProd[Prod9[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
      new FFunctor[Prod9[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]] with FTraverseProd[Prod9[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]] {
        def map[F[_], G[_]](p: Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9])(nt: F ~> G): Prod9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9] =
          Prod9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9]((nt(p.t1), nt(p.t2), nt(p.t3), nt(p.t4), nt(p.t5), nt(p.t6), nt(p.t7), nt(p.t8), nt(p.t9)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9])(f: F ~> Lambda[a => A[G[a]]]): A[Prod9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
          Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].map(f(p.t1))((i0: G[A1]) => (i1: G[A2]) => (i2: G[A3]) => (i3: G[A4]) => (i4: G[A5]) => (i5: G[A6]) => (i6: G[A7]) => (i7: G[A8]) => (i8: G[A9]) => Prod9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9]((i0, i1, i2, i3, i4, i5, i6, i7, i8))))(f(p.t2)))(f(p.t3)))(f(p.t4)))(f(p.t5)))(f(p.t6)))(f(p.t7)))(f(p.t8)))(f(p.t9))
      }

    implicit def Prod9FoldMap[A1, A2, A3, A4, A5, A6, A7, A8, A9]: FoldMap[Prod9[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9], Cop9[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
      new FoldMap[Prod9[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9], Cop9[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]] {
        def emptyProd[F[_]](implicit PE: MonoidK[F]): Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] =
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

            case Left(_) =>
              val (h, t) = U(p.t1)
              (h.map(v => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](Left(v))), Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9)))

            case Right(Left(_)) =>
              val (h, t) = U(p.t2)
              (h.map(v => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Left(v)))), Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, t, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9)))

            case Right(Right(Left(_))) =>
              val (h, t) = U(p.t3)
              (h.map(v => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Left(v))))), Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, t, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9)))

            case Right(Right(Right(Left(_)))) =>
              val (h, t) = U(p.t4)
              (h.map(v => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Left(v)))))), Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, t, p.t5, p.t6, p.t7, p.t8, p.t9)))

            case Right(Right(Right(Right(Left(_))))) =>
              val (h, t) = U(p.t5)
              (h.map(v => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Left(v))))))), Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, p.t4, t, p.t6, p.t7, p.t8, p.t9)))

            case Right(Right(Right(Right(Right(Left(_)))))) =>
              val (h, t) = U(p.t6)
              (h.map(v => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Right(Left(v)))))))), Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, p.t4, p.t5, t, p.t7, p.t8, p.t9)))

            case Right(Right(Right(Right(Right(Right(Left(_))))))) =>
              val (h, t) = U(p.t7)
              (h.map(v => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Right(Right(Left(v))))))))), Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, t, p.t8, p.t9)))

            case Right(Right(Right(Right(Right(Right(Right(Left(_)))))))) =>
              val (h, t) = U(p.t8)
              (h.map(v => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Right(Right(Right(Left(v)))))))))), Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, t, p.t9)))

            case Right(Right(Right(Right(Right(Right(Right(Right(_)))))))) =>
              val (h, t) = U(p.t9)
              (h.map(v => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Right(Right(Right(Right(v)))))))))), Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, t)))

          }
      }

    def Prod9TupleIso[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Iso[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9])] =
      Iso((_: Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]).run)(Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](_: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9])))

    implicit def Prod9Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9])]): Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = {
      val iso = Prod9TupleIso[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]
      M.imap(iso.reverseGet)(iso.get)
    }

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A1]] = {
      val t = M.empty
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((x, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9)))
    }

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A2]] = {
      val t = M.empty
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t.t1, x, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9)))
    }

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A3]] = {
      val t = M.empty
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t.t1, t.t2, x, t.t4, t.t5, t.t6, t.t7, t.t8, t.t9)))
    }

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A4]] = {
      val t = M.empty
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t.t1, t.t2, t.t3, x, t.t5, t.t6, t.t7, t.t8, t.t9)))
    }

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A5]] = {
      val t = M.empty
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t.t1, t.t2, t.t3, t.t4, x, t.t6, t.t7, t.t8, t.t9)))
    }

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A6]] = {
      val t = M.empty
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t.t1, t.t2, t.t3, t.t4, t.t5, x, t.t7, t.t8, t.t9)))
    }

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A7]] = {
      val t = M.empty
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, x, t.t8, t.t9)))
    }

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A8]] = {
      val t = M.empty
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, x, t.t9)))
    }

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A9]] = {
      val t = M.empty
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7, t.t8, x)))
    }

    implicit def injProdToVecCop[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Vector[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
      Inj.instance(p => Vector(
        Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Left(p.t1)),
        Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Left(p.t2))),
        Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Left(p.t3)))),
        Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Left(p.t4))))),
        Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Left(p.t5)))))),
        Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Right(Left(p.t6))))))),
        Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Right(Right(Left(p.t7)))))))),
        Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Right(Right(Right(Left(p.t8))))))))),
        Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Right(Right(Right(Right(p.t9)))))))))))

    implicit def Prod9Lens0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A1]] =
      Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A1]](p => p.t1)(x => p =>
        Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((x, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9)))

    implicit def Prod9Lens1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A2]] =
      Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A2]](p => p.t2)(x => p =>
        Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, x, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9)))

    implicit def Prod9Lens2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A3]] =
      Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A3]](p => p.t3)(x => p =>
        Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, x, p.t4, p.t5, p.t6, p.t7, p.t8, p.t9)))

    implicit def Prod9Lens3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A4]] =
      Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A4]](p => p.t4)(x => p =>
        Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, x, p.t5, p.t6, p.t7, p.t8, p.t9)))

    implicit def Prod9Lens4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A5]] =
      Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A5]](p => p.t5)(x => p =>
        Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, p.t4, x, p.t6, p.t7, p.t8, p.t9)))

    implicit def Prod9Lens5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A6]] =
      Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A6]](p => p.t6)(x => p =>
        Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, p.t4, p.t5, x, p.t7, p.t8, p.t9)))

    implicit def Prod9Lens6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A7]] =
      Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A7]](p => p.t7)(x => p =>
        Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, x, p.t8, p.t9)))

    implicit def Prod9Lens7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A8]] =
      Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A8]](p => p.t8)(x => p =>
        Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, x, p.t9)))

    implicit def Prod9Lens8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A9]] =
      Lens[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A9]](p => p.t9)(x => p =>
        Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7, p.t8, x)))

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

  @newtype case class Cop9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](run: Either[F[A1], Either[F[A2], Either[F[A3], Either[F[A4], Either[F[A5], Either[F[A6], Either[F[A7], Either[F[A8], F[A9]]]]]]]]]) {
    private def mapN = new Either9Ops[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9]](run)

    def map1[B](f: F[A1] => F[B]): Cop9[F, B, A2, A3, A4, A5, A6, A7, A8, A9] =
      Cop9[F, B, A2, A3, A4, A5, A6, A7, A8, A9](mapN.map1(f))

    def map2[B](f: F[A2] => F[B]): Cop9[F, A1, B, A3, A4, A5, A6, A7, A8, A9] =
      Cop9[F, A1, B, A3, A4, A5, A6, A7, A8, A9](mapN.map2(f))

    def map3[B](f: F[A3] => F[B]): Cop9[F, A1, A2, B, A4, A5, A6, A7, A8, A9] =
      Cop9[F, A1, A2, B, A4, A5, A6, A7, A8, A9](mapN.map3(f))

    def map4[B](f: F[A4] => F[B]): Cop9[F, A1, A2, A3, B, A5, A6, A7, A8, A9] =
      Cop9[F, A1, A2, A3, B, A5, A6, A7, A8, A9](mapN.map4(f))

    def map5[B](f: F[A5] => F[B]): Cop9[F, A1, A2, A3, A4, B, A6, A7, A8, A9] =
      Cop9[F, A1, A2, A3, A4, B, A6, A7, A8, A9](mapN.map5(f))

    def map6[B](f: F[A6] => F[B]): Cop9[F, A1, A2, A3, A4, A5, B, A7, A8, A9] =
      Cop9[F, A1, A2, A3, A4, A5, B, A7, A8, A9](mapN.map6(f))

    def map7[B](f: F[A7] => F[B]): Cop9[F, A1, A2, A3, A4, A5, A6, B, A8, A9] =
      Cop9[F, A1, A2, A3, A4, A5, A6, B, A8, A9](mapN.map7(f))

    def map8[B](f: F[A8] => F[B]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, B, A9] =
      Cop9[F, A1, A2, A3, A4, A5, A6, A7, B, A9](mapN.map8(f))

    def map9[B](f: F[A9] => F[B]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, B] =
      Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, B](mapN.map9(f))

  }

  trait Cop9LP {

    implicit def Cop9Instance[A1, A2, A3, A4, A5, A6, A7, A8, A9]: FFunctor[Cop9[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]] with FTraverseCop[Cop9[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
      new FFunctor[Cop9[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]] with FTraverseCop[Cop9[*[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]] {
        def map[F[_], G[_]](c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9])(nt: F ~> G): Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9] =
          Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](c.run.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_))))))))))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9])(f: F ~> Lambda[a => A[G[a]]]): A[Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
          c.run match {

            case Left(x) => Functor[A].map(f(x))(y => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](Left(y)))

            case Right(Left(x)) => Functor[A].map(f(x))(y => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Left(y))))

            case Right(Right(Left(x))) => Functor[A].map(f(x))(y => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Left(y)))))

            case Right(Right(Right(Left(x)))) => Functor[A].map(f(x))(y => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Left(y))))))

            case Right(Right(Right(Right(Left(x))))) => Functor[A].map(f(x))(y => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Left(y)))))))

            case Right(Right(Right(Right(Right(Left(x)))))) => Functor[A].map(f(x))(y => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Right(Left(y))))))))

            case Right(Right(Right(Right(Right(Right(Left(x))))))) => Functor[A].map(f(x))(y => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Right(Right(Left(y)))))))))

            case Right(Right(Right(Right(Right(Right(Right(Left(x)))))))) => Functor[A].map(f(x))(y => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Right(Right(Right(Left(y))))))))))

            case Right(Right(Right(Right(Right(Right(Right(Right(x)))))))) => Functor[A].map(f(x))(y => Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Right(Right(Right(Right(y))))))))))

          }
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A1]] =
      Inj.instance(x => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Left(x)))

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A2]] =
      Inj.instance(x => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Left(x))))

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A3]] =
      Inj.instance(x => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Left(x)))))

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A4]] =
      Inj.instance(x => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Left(x))))))

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A5]] =
      Inj.instance(x => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Left(x)))))))

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A6]] =
      Inj.instance(x => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Right(Left(x))))))))

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A7]] =
      Inj.instance(x => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Right(Right(Left(x)))))))))

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A8]] =
      Inj.instance(x => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A9]] =
      Inj.instance(x => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Right(Right(Right(Right(x))))))))))

    implicit def injCopToProd[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
      Inj.instance(_.run match {
        case Left(x) => Prod9.lifta0F[F, A1, A2, A3, A4, A5, A6, A7, A8, A9].apply(x)
        case Right(Left(x)) => Prod9.lifta1F[F, A1, A2, A3, A4, A5, A6, A7, A8, A9].apply(x)
        case Right(Right(Left(x))) => Prod9.lifta2F[F, A1, A2, A3, A4, A5, A6, A7, A8, A9].apply(x)
        case Right(Right(Right(Left(x)))) => Prod9.lifta3F[F, A1, A2, A3, A4, A5, A6, A7, A8, A9].apply(x)
        case Right(Right(Right(Right(Left(x))))) => Prod9.lifta4F[F, A1, A2, A3, A4, A5, A6, A7, A8, A9].apply(x)
        case Right(Right(Right(Right(Right(Left(x)))))) => Prod9.lifta5F[F, A1, A2, A3, A4, A5, A6, A7, A8, A9].apply(x)
        case Right(Right(Right(Right(Right(Right(Left(x))))))) => Prod9.lifta6F[F, A1, A2, A3, A4, A5, A6, A7, A8, A9].apply(x)
        case Right(Right(Right(Right(Right(Right(Right(Left(x)))))))) => Prod9.lifta7F[F, A1, A2, A3, A4, A5, A6, A7, A8, A9].apply(x)
        case Right(Right(Right(Right(Right(Right(Right(Right(x)))))))) => Prod9.lifta8F[F, A1, A2, A3, A4, A5, A6, A7, A8, A9].apply(x)
      })

    implicit def Cop9Optional0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Optional[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A1]] =
      Optional[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A1]](c => c.run match {
        case Left(x) => Some(x)
        case _ => None
      })(x => _ => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Left(x)))

    implicit def Cop9Optional1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Optional[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A2]] =
      Optional[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A2]](c => c.run match {
        case Right(Left(x)) => Some(x)
        case _ => None
      })(x => _ => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Left(x))))

    implicit def Cop9Optional2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Optional[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A3]] =
      Optional[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A3]](c => c.run match {
        case Right(Right(Left(x))) => Some(x)
        case _ => None
      })(x => _ => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Left(x)))))

    implicit def Cop9Optional3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Optional[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A4]] =
      Optional[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A4]](c => c.run match {
        case Right(Right(Right(Left(x)))) => Some(x)
        case _ => None
      })(x => _ => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Left(x))))))

    implicit def Cop9Optional4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Optional[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A5]] =
      Optional[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A5]](c => c.run match {
        case Right(Right(Right(Right(Left(x))))) => Some(x)
        case _ => None
      })(x => _ => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Left(x)))))))

    implicit def Cop9Optional5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Optional[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A6]] =
      Optional[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A6]](c => c.run match {
        case Right(Right(Right(Right(Right(Left(x)))))) => Some(x)
        case _ => None
      })(x => _ => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Right(Left(x))))))))

    implicit def Cop9Optional6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Optional[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A7]] =
      Optional[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A7]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Left(x))))))) => Some(x)
        case _ => None
      })(x => _ => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Right(Right(Left(x)))))))))

    implicit def Cop9Optional7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Optional[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A8]] =
      Optional[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A8]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Right(Left(x)))))))) => Some(x)
        case _ => None
      })(x => _ => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Right(Right(Right(Left(x))))))))))

    implicit def Cop9Optional8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Optional[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A9]] =
      Optional[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A9]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(Right(Right(x)))))))) => Some(x)
        case _ => None
      })(x => _ => Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](Right(Right(Right(Right(Right(Right(Right(Right(x))))))))))

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

    implicit def Cop9Optional0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Optional[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A1] =
      Cop9Optional0[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Cop9Optional1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Optional[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A2] =
      Cop9Optional1[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Cop9Optional2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Optional[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A3] =
      Cop9Optional2[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Cop9Optional3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Optional[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A4] =
      Cop9Optional3[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Cop9Optional4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Optional[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A5] =
      Cop9Optional4[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Cop9Optional5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Optional[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A6] =
      Cop9Optional5[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Cop9Optional6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Optional[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A7] =
      Cop9Optional6[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Cop9Optional7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Optional[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A8] =
      Cop9Optional7[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def Cop9Optional8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Optional[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A9] =
      Cop9Optional8[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

  }
}
