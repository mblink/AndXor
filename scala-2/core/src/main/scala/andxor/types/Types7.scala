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

trait Types7 {
  final type Prod7[F[_], A1, A2, A3, A4, A5, A6, A7] = Types7.Prod7[F, A1, A2, A3, A4, A5, A6, A7]
  final val Prod7: Types7.Prod7.type = Types7.Prod7
  final type Cop7[F[_], A1, A2, A3, A4, A5, A6, A7] = Types7.Cop7[F, A1, A2, A3, A4, A5, A6, A7]
  final val Cop7: Types7.Cop7.type = Types7.Cop7
}

object Types7 {
  @newtype case class Prod7[F[_], A1, A2, A3, A4, A5, A6, A7](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7])) { self =>
    def t1: F[A1] = run._1
    def t2: F[A2] = run._2
    def t3: F[A3] = run._3
    def t4: F[A4] = run._4
    def t5: F[A5] = run._5
    def t6: F[A6] = run._6
    def t7: F[A7] = run._7

    private def mapN = new Tuple7Ops[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7]](run)

    def map1[B](f: F[A1] => F[B]): Prod7[F, B, A2, A3, A4, A5, A6, A7] = {
      Prod7[F, B, A2, A3, A4, A5, A6, A7](mapN.map1(f))
    }

    def map2[B](f: F[A2] => F[B]): Prod7[F, A1, B, A3, A4, A5, A6, A7] = {
      Prod7[F, A1, B, A3, A4, A5, A6, A7](mapN.map2(f))
    }

    def map3[B](f: F[A3] => F[B]): Prod7[F, A1, A2, B, A4, A5, A6, A7] = {
      Prod7[F, A1, A2, B, A4, A5, A6, A7](mapN.map3(f))
    }

    def map4[B](f: F[A4] => F[B]): Prod7[F, A1, A2, A3, B, A5, A6, A7] = {
      Prod7[F, A1, A2, A3, B, A5, A6, A7](mapN.map4(f))
    }

    def map5[B](f: F[A5] => F[B]): Prod7[F, A1, A2, A3, A4, B, A6, A7] = {
      Prod7[F, A1, A2, A3, A4, B, A6, A7](mapN.map5(f))
    }

    def map6[B](f: F[A6] => F[B]): Prod7[F, A1, A2, A3, A4, A5, B, A7] = {
      Prod7[F, A1, A2, A3, A4, A5, B, A7](mapN.map6(f))
    }

    def map7[B](f: F[A7] => F[B]): Prod7[F, A1, A2, A3, A4, A5, A6, B] = {
      Prod7[F, A1, A2, A3, A4, A5, A6, B](mapN.map7(f))
    }

  }

  trait Prod7LP {

    implicit def Prod7Instance[A1, A2, A3, A4, A5, A6, A7]: FFunctor[Prod7[*[_], A1, A2, A3, A4, A5, A6, A7]] with FTraverseProd[Prod7[*[_], A1, A2, A3, A4, A5, A6, A7]] =
      new FFunctor[Prod7[*[_], A1, A2, A3, A4, A5, A6, A7]] with FTraverseProd[Prod7[*[_], A1, A2, A3, A4, A5, A6, A7]] {
        def map[F[_], G[_]](p: Prod7[F, A1, A2, A3, A4, A5, A6, A7])(nt: F ~> G): Prod7[G, A1, A2, A3, A4, A5, A6, A7] =
          Prod7[G, A1, A2, A3, A4, A5, A6, A7]((nt(p.t1), nt(p.t2), nt(p.t3), nt(p.t4), nt(p.t5), nt(p.t6), nt(p.t7)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod7[F, A1, A2, A3, A4, A5, A6, A7])(f: F ~> Lambda[a => A[G[a]]]): A[Prod7[G, A1, A2, A3, A4, A5, A6, A7]] =
          Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].map(f(p.t1))((i0: G[A1]) => (i1: G[A2]) => (i2: G[A3]) => (i3: G[A4]) => (i4: G[A5]) => (i5: G[A6]) => (i6: G[A7]) => Prod7[G, A1, A2, A3, A4, A5, A6, A7]((i0, i1, i2, i3, i4, i5, i6))))(f(p.t2)))(f(p.t3)))(f(p.t4)))(f(p.t5)))(f(p.t6)))(f(p.t7))
      }

    implicit def Prod7FoldMap[A1, A2, A3, A4, A5, A6, A7]: FoldMap[Prod7[*[_], A1, A2, A3, A4, A5, A6, A7], Cop7[*[_], A1, A2, A3, A4, A5, A6, A7]] =
      new FoldMap[Prod7[*[_], A1, A2, A3, A4, A5, A6, A7], Cop7[*[_], A1, A2, A3, A4, A5, A6, A7]] {
        def emptyProd[F[_]](implicit PE: MonoidK[F]): Prod7[F, A1, A2, A3, A4, A5, A6, A7] =
          Prod7[F, A1, A2, A3, A4, A5, A6, A7]((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5], PE.empty[A6], PE.empty[A7]))

        def unconsAll[F[_], G[_]](p: Prod7[F, A1, A2, A3, A4, A5, A6, A7])(implicit U: Uncons[F, G]): (List[Cop7[G, A1, A2, A3, A4, A5, A6, A7]], Prod7[F, A1, A2, A3, A4, A5, A6, A7]) = {
          val (h1, t1) = U(p.t1)
          val (h2, t2) = U(p.t2)
          val (h3, t3) = U(p.t3)
          val (h4, t4) = U(p.t4)
          val (h5, t5) = U(p.t5)
          val (h6, t6) = U(p.t6)
          val (h7, t7) = U(p.t7)
          (
            List(h1.map(Inj[Cop7[G, A1, A2, A3, A4, A5, A6, A7], G[A1]].apply(_)), h2.map(Inj[Cop7[G, A1, A2, A3, A4, A5, A6, A7], G[A2]].apply(_)), h3.map(Inj[Cop7[G, A1, A2, A3, A4, A5, A6, A7], G[A3]].apply(_)), h4.map(Inj[Cop7[G, A1, A2, A3, A4, A5, A6, A7], G[A4]].apply(_)), h5.map(Inj[Cop7[G, A1, A2, A3, A4, A5, A6, A7], G[A5]].apply(_)), h6.map(Inj[Cop7[G, A1, A2, A3, A4, A5, A6, A7], G[A6]].apply(_)), h7.map(Inj[Cop7[G, A1, A2, A3, A4, A5, A6, A7], G[A7]].apply(_))).flatten,
            Prod7[F, A1, A2, A3, A4, A5, A6, A7]((t1, t2, t3, t4, t5, t6, t7)))
        }

        def unconsOne[F[_], G[_]](p: Prod7[F, A1, A2, A3, A4, A5, A6, A7], c: Cop7[G, A1, A2, A3, A4, A5, A6, A7])(implicit U: Uncons[F, G]): (Option[Cop7[G, A1, A2, A3, A4, A5, A6, A7]], Prod7[F, A1, A2, A3, A4, A5, A6, A7]) =
          c.run match {

            case Left(_) =>
              val (h, t) = U(p.t1)
              (h.map(v => Cop7[G, A1, A2, A3, A4, A5, A6, A7](Left(v))), Prod7[F, A1, A2, A3, A4, A5, A6, A7]((t, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7)))

            case Right(Left(_)) =>
              val (h, t) = U(p.t2)
              (h.map(v => Cop7[G, A1, A2, A3, A4, A5, A6, A7](Right(Left(v)))), Prod7[F, A1, A2, A3, A4, A5, A6, A7]((p.t1, t, p.t3, p.t4, p.t5, p.t6, p.t7)))

            case Right(Right(Left(_))) =>
              val (h, t) = U(p.t3)
              (h.map(v => Cop7[G, A1, A2, A3, A4, A5, A6, A7](Right(Right(Left(v))))), Prod7[F, A1, A2, A3, A4, A5, A6, A7]((p.t1, p.t2, t, p.t4, p.t5, p.t6, p.t7)))

            case Right(Right(Right(Left(_)))) =>
              val (h, t) = U(p.t4)
              (h.map(v => Cop7[G, A1, A2, A3, A4, A5, A6, A7](Right(Right(Right(Left(v)))))), Prod7[F, A1, A2, A3, A4, A5, A6, A7]((p.t1, p.t2, p.t3, t, p.t5, p.t6, p.t7)))

            case Right(Right(Right(Right(Left(_))))) =>
              val (h, t) = U(p.t5)
              (h.map(v => Cop7[G, A1, A2, A3, A4, A5, A6, A7](Right(Right(Right(Right(Left(v))))))), Prod7[F, A1, A2, A3, A4, A5, A6, A7]((p.t1, p.t2, p.t3, p.t4, t, p.t6, p.t7)))

            case Right(Right(Right(Right(Right(Left(_)))))) =>
              val (h, t) = U(p.t6)
              (h.map(v => Cop7[G, A1, A2, A3, A4, A5, A6, A7](Right(Right(Right(Right(Right(Left(v)))))))), Prod7[F, A1, A2, A3, A4, A5, A6, A7]((p.t1, p.t2, p.t3, p.t4, p.t5, t, p.t7)))

            case Right(Right(Right(Right(Right(Right(_)))))) =>
              val (h, t) = U(p.t7)
              (h.map(v => Cop7[G, A1, A2, A3, A4, A5, A6, A7](Right(Right(Right(Right(Right(Right(v)))))))), Prod7[F, A1, A2, A3, A4, A5, A6, A7]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, t)))

          }
      }

    def Prod7TupleIso[F[_], A1, A2, A3, A4, A5, A6, A7]: Iso[Prod7[F, A1, A2, A3, A4, A5, A6, A7], (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7])] =
      Iso((_: Prod7[F, A1, A2, A3, A4, A5, A6, A7]).run)(Prod7[F, A1, A2, A3, A4, A5, A6, A7](_: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7])))

    implicit def Prod7Monoid[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7])]): Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]] = {
      val iso = Prod7TupleIso[F, A1, A2, A3, A4, A5, A6, A7]
      M.imap(iso.reverseGet)(iso.get)
    }

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A1]] = {
      val t = M.empty
      Inj.instance(x => Prod7[F, A1, A2, A3, A4, A5, A6, A7]((x, t.t2, t.t3, t.t4, t.t5, t.t6, t.t7)))
    }

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A2]] = {
      val t = M.empty
      Inj.instance(x => Prod7[F, A1, A2, A3, A4, A5, A6, A7]((t.t1, x, t.t3, t.t4, t.t5, t.t6, t.t7)))
    }

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A3]] = {
      val t = M.empty
      Inj.instance(x => Prod7[F, A1, A2, A3, A4, A5, A6, A7]((t.t1, t.t2, x, t.t4, t.t5, t.t6, t.t7)))
    }

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A4]] = {
      val t = M.empty
      Inj.instance(x => Prod7[F, A1, A2, A3, A4, A5, A6, A7]((t.t1, t.t2, t.t3, x, t.t5, t.t6, t.t7)))
    }

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A5]] = {
      val t = M.empty
      Inj.instance(x => Prod7[F, A1, A2, A3, A4, A5, A6, A7]((t.t1, t.t2, t.t3, t.t4, x, t.t6, t.t7)))
    }

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A6]] = {
      val t = M.empty
      Inj.instance(x => Prod7[F, A1, A2, A3, A4, A5, A6, A7]((t.t1, t.t2, t.t3, t.t4, t.t5, x, t.t7)))
    }

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A7]] = {
      val t = M.empty
      Inj.instance(x => Prod7[F, A1, A2, A3, A4, A5, A6, A7]((t.t1, t.t2, t.t3, t.t4, t.t5, t.t6, x)))
    }

    implicit def injProdToVecCop[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Vector[Cop7[F, A1, A2, A3, A4, A5, A6, A7]], Prod7[F, A1, A2, A3, A4, A5, A6, A7]] =
      Inj.instance(p => Vector(
        Cop7[F, A1, A2, A3, A4, A5, A6, A7](Left(p.t1)),
        Cop7[F, A1, A2, A3, A4, A5, A6, A7](Right(Left(p.t2))),
        Cop7[F, A1, A2, A3, A4, A5, A6, A7](Right(Right(Left(p.t3)))),
        Cop7[F, A1, A2, A3, A4, A5, A6, A7](Right(Right(Right(Left(p.t4))))),
        Cop7[F, A1, A2, A3, A4, A5, A6, A7](Right(Right(Right(Right(Left(p.t5)))))),
        Cop7[F, A1, A2, A3, A4, A5, A6, A7](Right(Right(Right(Right(Right(Left(p.t6))))))),
        Cop7[F, A1, A2, A3, A4, A5, A6, A7](Right(Right(Right(Right(Right(Right(p.t7)))))))))

    implicit def Prod7Lens0[F[_], A1, A2, A3, A4, A5, A6, A7]: Lens[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A1]] =
      Lens[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A1]](p => p.t1)(x => p =>
        Prod7[F, A1, A2, A3, A4, A5, A6, A7]((x, p.t2, p.t3, p.t4, p.t5, p.t6, p.t7)))

    implicit def Prod7Lens1[F[_], A1, A2, A3, A4, A5, A6, A7]: Lens[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A2]] =
      Lens[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A2]](p => p.t2)(x => p =>
        Prod7[F, A1, A2, A3, A4, A5, A6, A7]((p.t1, x, p.t3, p.t4, p.t5, p.t6, p.t7)))

    implicit def Prod7Lens2[F[_], A1, A2, A3, A4, A5, A6, A7]: Lens[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A3]] =
      Lens[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A3]](p => p.t3)(x => p =>
        Prod7[F, A1, A2, A3, A4, A5, A6, A7]((p.t1, p.t2, x, p.t4, p.t5, p.t6, p.t7)))

    implicit def Prod7Lens3[F[_], A1, A2, A3, A4, A5, A6, A7]: Lens[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A4]] =
      Lens[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A4]](p => p.t4)(x => p =>
        Prod7[F, A1, A2, A3, A4, A5, A6, A7]((p.t1, p.t2, p.t3, x, p.t5, p.t6, p.t7)))

    implicit def Prod7Lens4[F[_], A1, A2, A3, A4, A5, A6, A7]: Lens[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A5]] =
      Lens[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A5]](p => p.t5)(x => p =>
        Prod7[F, A1, A2, A3, A4, A5, A6, A7]((p.t1, p.t2, p.t3, p.t4, x, p.t6, p.t7)))

    implicit def Prod7Lens5[F[_], A1, A2, A3, A4, A5, A6, A7]: Lens[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A6]] =
      Lens[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A6]](p => p.t6)(x => p =>
        Prod7[F, A1, A2, A3, A4, A5, A6, A7]((p.t1, p.t2, p.t3, p.t4, p.t5, x, p.t7)))

    implicit def Prod7Lens6[F[_], A1, A2, A3, A4, A5, A6, A7]: Lens[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A7]] =
      Lens[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A7]](p => p.t7)(x => p =>
        Prod7[F, A1, A2, A3, A4, A5, A6, A7]((p.t1, p.t2, p.t3, p.t4, p.t5, p.t6, x)))

  }

  object Prod7 extends Prod7LP {

    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[Id, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[Id, A1, A2, A3, A4, A5, A6, A7], A1] =
      lifta0F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[Id, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[Id, A1, A2, A3, A4, A5, A6, A7], A2] =
      lifta1F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[Id, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[Id, A1, A2, A3, A4, A5, A6, A7], A3] =
      lifta2F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[Id, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[Id, A1, A2, A3, A4, A5, A6, A7], A4] =
      lifta3F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[Id, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[Id, A1, A2, A3, A4, A5, A6, A7], A5] =
      lifta4F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[Id, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[Id, A1, A2, A3, A4, A5, A6, A7], A6] =
      lifta5F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[Id, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[Id, A1, A2, A3, A4, A5, A6, A7], A7] =
      lifta6F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def Prod7Lens0Id[A1, A2, A3, A4, A5, A6, A7]: Lens[Prod7[Id, A1, A2, A3, A4, A5, A6, A7], A1] =
      Prod7Lens0[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def Prod7Lens1Id[A1, A2, A3, A4, A5, A6, A7]: Lens[Prod7[Id, A1, A2, A3, A4, A5, A6, A7], A2] =
      Prod7Lens1[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def Prod7Lens2Id[A1, A2, A3, A4, A5, A6, A7]: Lens[Prod7[Id, A1, A2, A3, A4, A5, A6, A7], A3] =
      Prod7Lens2[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def Prod7Lens3Id[A1, A2, A3, A4, A5, A6, A7]: Lens[Prod7[Id, A1, A2, A3, A4, A5, A6, A7], A4] =
      Prod7Lens3[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def Prod7Lens4Id[A1, A2, A3, A4, A5, A6, A7]: Lens[Prod7[Id, A1, A2, A3, A4, A5, A6, A7], A5] =
      Prod7Lens4[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def Prod7Lens5Id[A1, A2, A3, A4, A5, A6, A7]: Lens[Prod7[Id, A1, A2, A3, A4, A5, A6, A7], A6] =
      Prod7Lens5[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def Prod7Lens6Id[A1, A2, A3, A4, A5, A6, A7]: Lens[Prod7[Id, A1, A2, A3, A4, A5, A6, A7], A7] =
      Prod7Lens6[Id, A1, A2, A3, A4, A5, A6, A7]

  }

  @newtype case class Cop7[F[_], A1, A2, A3, A4, A5, A6, A7](run: Either[F[A1], Either[F[A2], Either[F[A3], Either[F[A4], Either[F[A5], Either[F[A6], F[A7]]]]]]]) {
    private def mapN = new Either7Ops[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7]](run)

    def map1[B](f: F[A1] => F[B]): Cop7[F, B, A2, A3, A4, A5, A6, A7] =
      Cop7[F, B, A2, A3, A4, A5, A6, A7](mapN.map1(f))

    def map2[B](f: F[A2] => F[B]): Cop7[F, A1, B, A3, A4, A5, A6, A7] =
      Cop7[F, A1, B, A3, A4, A5, A6, A7](mapN.map2(f))

    def map3[B](f: F[A3] => F[B]): Cop7[F, A1, A2, B, A4, A5, A6, A7] =
      Cop7[F, A1, A2, B, A4, A5, A6, A7](mapN.map3(f))

    def map4[B](f: F[A4] => F[B]): Cop7[F, A1, A2, A3, B, A5, A6, A7] =
      Cop7[F, A1, A2, A3, B, A5, A6, A7](mapN.map4(f))

    def map5[B](f: F[A5] => F[B]): Cop7[F, A1, A2, A3, A4, B, A6, A7] =
      Cop7[F, A1, A2, A3, A4, B, A6, A7](mapN.map5(f))

    def map6[B](f: F[A6] => F[B]): Cop7[F, A1, A2, A3, A4, A5, B, A7] =
      Cop7[F, A1, A2, A3, A4, A5, B, A7](mapN.map6(f))

    def map7[B](f: F[A7] => F[B]): Cop7[F, A1, A2, A3, A4, A5, A6, B] =
      Cop7[F, A1, A2, A3, A4, A5, A6, B](mapN.map7(f))

  }

  trait Cop7LP {

    implicit def Cop7Instance[A1, A2, A3, A4, A5, A6, A7]: FFunctor[Cop7[*[_], A1, A2, A3, A4, A5, A6, A7]] with FTraverseCop[Cop7[*[_], A1, A2, A3, A4, A5, A6, A7]] =
      new FFunctor[Cop7[*[_], A1, A2, A3, A4, A5, A6, A7]] with FTraverseCop[Cop7[*[_], A1, A2, A3, A4, A5, A6, A7]] {
        def map[F[_], G[_]](c: Cop7[F, A1, A2, A3, A4, A5, A6, A7])(nt: F ~> G): Cop7[G, A1, A2, A3, A4, A5, A6, A7] =
          Cop7[G, A1, A2, A3, A4, A5, A6, A7](c.run.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_))))))))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop7[F, A1, A2, A3, A4, A5, A6, A7])(f: F ~> Lambda[a => A[G[a]]]): A[Cop7[G, A1, A2, A3, A4, A5, A6, A7]] =
          c.run match {

            case Left(x) => Functor[A].map(f(x))(y => Cop7[G, A1, A2, A3, A4, A5, A6, A7](Left(y)))

            case Right(Left(x)) => Functor[A].map(f(x))(y => Cop7[G, A1, A2, A3, A4, A5, A6, A7](Right(Left(y))))

            case Right(Right(Left(x))) => Functor[A].map(f(x))(y => Cop7[G, A1, A2, A3, A4, A5, A6, A7](Right(Right(Left(y)))))

            case Right(Right(Right(Left(x)))) => Functor[A].map(f(x))(y => Cop7[G, A1, A2, A3, A4, A5, A6, A7](Right(Right(Right(Left(y))))))

            case Right(Right(Right(Right(Left(x))))) => Functor[A].map(f(x))(y => Cop7[G, A1, A2, A3, A4, A5, A6, A7](Right(Right(Right(Right(Left(y)))))))

            case Right(Right(Right(Right(Right(Left(x)))))) => Functor[A].map(f(x))(y => Cop7[G, A1, A2, A3, A4, A5, A6, A7](Right(Right(Right(Right(Right(Left(y))))))))

            case Right(Right(Right(Right(Right(Right(x)))))) => Functor[A].map(f(x))(y => Cop7[G, A1, A2, A3, A4, A5, A6, A7](Right(Right(Right(Right(Right(Right(y))))))))

          }
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A1]] =
      Inj.instance(x => Cop7[F, A1, A2, A3, A4, A5, A6, A7](Left(x)))

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A2]] =
      Inj.instance(x => Cop7[F, A1, A2, A3, A4, A5, A6, A7](Right(Left(x))))

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A3]] =
      Inj.instance(x => Cop7[F, A1, A2, A3, A4, A5, A6, A7](Right(Right(Left(x)))))

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A4]] =
      Inj.instance(x => Cop7[F, A1, A2, A3, A4, A5, A6, A7](Right(Right(Right(Left(x))))))

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A5]] =
      Inj.instance(x => Cop7[F, A1, A2, A3, A4, A5, A6, A7](Right(Right(Right(Right(Left(x)))))))

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A6]] =
      Inj.instance(x => Cop7[F, A1, A2, A3, A4, A5, A6, A7](Right(Right(Right(Right(Right(Left(x))))))))

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A7]] =
      Inj.instance(x => Cop7[F, A1, A2, A3, A4, A5, A6, A7](Right(Right(Right(Right(Right(Right(x))))))))

    implicit def injCopToProd[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], Cop7[F, A1, A2, A3, A4, A5, A6, A7]] =
      Inj.instance(_.run match {
        case Left(x) => Prod7.lifta0F[F, A1, A2, A3, A4, A5, A6, A7].apply(x)
        case Right(Left(x)) => Prod7.lifta1F[F, A1, A2, A3, A4, A5, A6, A7].apply(x)
        case Right(Right(Left(x))) => Prod7.lifta2F[F, A1, A2, A3, A4, A5, A6, A7].apply(x)
        case Right(Right(Right(Left(x)))) => Prod7.lifta3F[F, A1, A2, A3, A4, A5, A6, A7].apply(x)
        case Right(Right(Right(Right(Left(x))))) => Prod7.lifta4F[F, A1, A2, A3, A4, A5, A6, A7].apply(x)
        case Right(Right(Right(Right(Right(Left(x)))))) => Prod7.lifta5F[F, A1, A2, A3, A4, A5, A6, A7].apply(x)
        case Right(Right(Right(Right(Right(Right(x)))))) => Prod7.lifta6F[F, A1, A2, A3, A4, A5, A6, A7].apply(x)
      })

    implicit def Cop7Optional0[F[_], A1, A2, A3, A4, A5, A6, A7]: Optional[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A1]] =
      Optional[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A1]](c => c.run match {
        case Left(x) => Some(x)
        case _ => None
      })(x => _ => Cop7[F, A1, A2, A3, A4, A5, A6, A7](Left(x)))

    implicit def Cop7Optional1[F[_], A1, A2, A3, A4, A5, A6, A7]: Optional[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A2]] =
      Optional[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A2]](c => c.run match {
        case Right(Left(x)) => Some(x)
        case _ => None
      })(x => _ => Cop7[F, A1, A2, A3, A4, A5, A6, A7](Right(Left(x))))

    implicit def Cop7Optional2[F[_], A1, A2, A3, A4, A5, A6, A7]: Optional[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A3]] =
      Optional[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A3]](c => c.run match {
        case Right(Right(Left(x))) => Some(x)
        case _ => None
      })(x => _ => Cop7[F, A1, A2, A3, A4, A5, A6, A7](Right(Right(Left(x)))))

    implicit def Cop7Optional3[F[_], A1, A2, A3, A4, A5, A6, A7]: Optional[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A4]] =
      Optional[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A4]](c => c.run match {
        case Right(Right(Right(Left(x)))) => Some(x)
        case _ => None
      })(x => _ => Cop7[F, A1, A2, A3, A4, A5, A6, A7](Right(Right(Right(Left(x))))))

    implicit def Cop7Optional4[F[_], A1, A2, A3, A4, A5, A6, A7]: Optional[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A5]] =
      Optional[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A5]](c => c.run match {
        case Right(Right(Right(Right(Left(x))))) => Some(x)
        case _ => None
      })(x => _ => Cop7[F, A1, A2, A3, A4, A5, A6, A7](Right(Right(Right(Right(Left(x)))))))

    implicit def Cop7Optional5[F[_], A1, A2, A3, A4, A5, A6, A7]: Optional[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A6]] =
      Optional[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A6]](c => c.run match {
        case Right(Right(Right(Right(Right(Left(x)))))) => Some(x)
        case _ => None
      })(x => _ => Cop7[F, A1, A2, A3, A4, A5, A6, A7](Right(Right(Right(Right(Right(Left(x))))))))

    implicit def Cop7Optional6[F[_], A1, A2, A3, A4, A5, A6, A7]: Optional[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A7]] =
      Optional[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A7]](c => c.run match {
        case Right(Right(Right(Right(Right(Right(x)))))) => Some(x)
        case _ => None
      })(x => _ => Cop7[F, A1, A2, A3, A4, A5, A6, A7](Right(Right(Right(Right(Right(Right(x))))))))

  }

  object Cop7 extends Cop7LP {

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A1] =
      inja0F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A2] =
      inja1F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A3] =
      inja2F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A4] =
      inja3F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A5] =
      inja4F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A6] =
      inja5F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A7] =
      inja6F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def Cop7Optional0Id[A1, A2, A3, A4, A5, A6, A7]: Optional[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A1] =
      Cop7Optional0[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def Cop7Optional1Id[A1, A2, A3, A4, A5, A6, A7]: Optional[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A2] =
      Cop7Optional1[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def Cop7Optional2Id[A1, A2, A3, A4, A5, A6, A7]: Optional[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A3] =
      Cop7Optional2[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def Cop7Optional3Id[A1, A2, A3, A4, A5, A6, A7]: Optional[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A4] =
      Cop7Optional3[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def Cop7Optional4Id[A1, A2, A3, A4, A5, A6, A7]: Optional[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A5] =
      Cop7Optional4[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def Cop7Optional5Id[A1, A2, A3, A4, A5, A6, A7]: Optional[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A6] =
      Cop7Optional5[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def Cop7Optional6Id[A1, A2, A3, A4, A5, A6, A7]: Optional[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A7] =
      Cop7Optional6[Id, A1, A2, A3, A4, A5, A6, A7]

  }
}
