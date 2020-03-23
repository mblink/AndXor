package andxor.types

import andxor._
import monocle.{Lens, Optional}
import cats.{~>, Applicative, Functor, Id, Monoid, MonoidK}
import cats.syntax.either._
import monocle.Iso

trait Types4 {
  @newtype case class Prod4[F[_], A1, A2, A3, A4](run: (F[A1], F[A2], F[A3], F[A4])) { self =>
    def t1: F[A1] = run._1
    def t2: F[A2] = run._2
    def t3: F[A3] = run._3
    def t4: F[A4] = run._4

    private def mapN = new Map4P[F[A1], F[A2], F[A3], F[A4]] {}

    def map1[B](f: F[A1] => F[B]): Prod4[F, B, A2, A3, A4] =
      Prod4[F, B, A2, A3, A4](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Prod4[F, B, A2, A3, A4] =
      Prod4[F, B, A2, A3, A4](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Prod4[F, A1, B, A3, A4] =
      Prod4[F, A1, B, A3, A4](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Prod4[F, A1, B, A3, A4] =
      Prod4[F, A1, B, A3, A4](mapN.mapAt(f)(run))

    def map3[B](f: F[A3] => F[B]): Prod4[F, A1, A2, B, A4] =
      Prod4[F, A1, A2, B, A4](mapN.map3(run)(f))

    def mapAt[B](f: F[A3] => F[B])(implicit d: Dummy3): Prod4[F, A1, A2, B, A4] =
      Prod4[F, A1, A2, B, A4](mapN.mapAt(f)(run))

    def map4[B](f: F[A4] => F[B]): Prod4[F, A1, A2, A3, B] =
      Prod4[F, A1, A2, A3, B](mapN.map4(run)(f))

    def mapAt[B](f: F[A4] => F[B])(implicit d: Dummy4): Prod4[F, A1, A2, A3, B] =
      Prod4[F, A1, A2, A3, B](mapN.mapAt(f)(run))

  }

  trait Prod4LP {

    implicit def Prod4Instance[A1, A2, A3, A4]: FFunctor[Prod4[?[_], A1, A2, A3, A4]] with FTraverseProd[Prod4[?[_], A1, A2, A3, A4]] =
      new FFunctor[Prod4[?[_], A1, A2, A3, A4]] with FTraverseProd[Prod4[?[_], A1, A2, A3, A4]] {
        def map[F[_], G[_]](p: Prod4[F, A1, A2, A3, A4])(nt: F ~> G): Prod4[G, A1, A2, A3, A4] =
          Prod4[G, A1, A2, A3, A4]((nt(p.t1), nt(p.t2), nt(p.t3), nt(p.t4)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod4[F, A1, A2, A3, A4])(f: F ~> Lambda[a => A[G[a]]]): A[Prod4[G, A1, A2, A3, A4]] =
          Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].map(f(p.t1))((i0: G[A1]) => (i1: G[A2]) => (i2: G[A3]) => (i3: G[A4]) => Prod4[G, A1, A2, A3, A4]((i0, i1, i2, i3))))(f(p.t2)))(f(p.t3)))(f(p.t4))
      }

    implicit def Prod4FoldMap[A1, A2, A3, A4]: FoldMap[Prod4[?[_], A1, A2, A3, A4], Cop4[?[_], A1, A2, A3, A4]] =
      new FoldMap[Prod4[?[_], A1, A2, A3, A4], Cop4[?[_], A1, A2, A3, A4]] {
        def emptyProd[F[_]](implicit PE: MonoidK[F]): Prod4[F, A1, A2, A3, A4] =
          Prod4[F, A1, A2, A3, A4]((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4]))

        def unconsAll[F[_], G[_]](p: Prod4[F, A1, A2, A3, A4])(implicit U: Uncons[F, G]): (List[Cop4[G, A1, A2, A3, A4]], Prod4[F, A1, A2, A3, A4]) = {
          val (h1, t1) = U(p.t1)
          val (h2, t2) = U(p.t2)
          val (h3, t3) = U(p.t3)
          val (h4, t4) = U(p.t4)
          (
            List(h1.map(Inj[Cop4[G, A1, A2, A3, A4], G[A1]].apply(_)), h2.map(Inj[Cop4[G, A1, A2, A3, A4], G[A2]].apply(_)), h3.map(Inj[Cop4[G, A1, A2, A3, A4], G[A3]].apply(_)), h4.map(Inj[Cop4[G, A1, A2, A3, A4], G[A4]].apply(_))).flatten,
            Prod4[F, A1, A2, A3, A4]((t1, t2, t3, t4)))
        }

        def unconsOne[F[_], G[_]](p: Prod4[F, A1, A2, A3, A4], c: Cop4[G, A1, A2, A3, A4])(implicit U: Uncons[F, G]): (Option[Cop4[G, A1, A2, A3, A4]], Prod4[F, A1, A2, A3, A4]) =
          c.run match {

            case Left(_) =>
              val (h, t) = U(p.t1)
              (h.map(v => Cop4[G, A1, A2, A3, A4](Left(v))), Prod4[F, A1, A2, A3, A4]((t, p.t2, p.t3, p.t4)))

            case Right(Left(_)) =>
              val (h, t) = U(p.t2)
              (h.map(v => Cop4[G, A1, A2, A3, A4](Right(Left(v)))), Prod4[F, A1, A2, A3, A4]((p.t1, t, p.t3, p.t4)))

            case Right(Right(Left(_))) =>
              val (h, t) = U(p.t3)
              (h.map(v => Cop4[G, A1, A2, A3, A4](Right(Right(Left(v))))), Prod4[F, A1, A2, A3, A4]((p.t1, p.t2, t, p.t4)))

            case Right(Right(Right(_))) =>
              val (h, t) = U(p.t4)
              (h.map(v => Cop4[G, A1, A2, A3, A4](Right(Right(Right(v))))), Prod4[F, A1, A2, A3, A4]((p.t1, p.t2, p.t3, t)))

          }
      }

    def Prod4TupleIso[F[_], A1, A2, A3, A4]: Iso[Prod4[F, A1, A2, A3, A4], (F[A1], F[A2], F[A3], F[A4])] =
      Iso((_: Prod4[F, A1, A2, A3, A4]).run)(Prod4[F, A1, A2, A3, A4](_: (F[A1], F[A2], F[A3], F[A4])))

    implicit def Prod4Monoid[F[_], A1, A2, A3, A4](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4])]): Monoid[Prod4[F, A1, A2, A3, A4]] =
      new Monoid[Prod4[F, A1, A2, A3, A4]] {
        lazy val iso = Prod4TupleIso[F, A1, A2, A3, A4]
        def empty: Prod4[F, A1, A2, A3, A4] = iso.reverseGet(M.empty)
        def combine(p1: Prod4[F, A1, A2, A3, A4], p2: Prod4[F, A1, A2, A3, A4]): Prod4[F, A1, A2, A3, A4] =
          iso.reverseGet(M.combine(iso.get(p1), iso.get(p2)))
      }

    implicit def lifta0F[F[_], A1, A2, A3, A4](implicit M: Monoid[Prod4[F, A1, A2, A3, A4]]): Inj[Prod4[F, A1, A2, A3, A4], F[A1]] = {
      val t = M.empty
      Inj.instance(x => Prod4[F, A1, A2, A3, A4]((x, t.t2, t.t3, t.t4)))
    }

    implicit def lifta1F[F[_], A1, A2, A3, A4](implicit M: Monoid[Prod4[F, A1, A2, A3, A4]]): Inj[Prod4[F, A1, A2, A3, A4], F[A2]] = {
      val t = M.empty
      Inj.instance(x => Prod4[F, A1, A2, A3, A4]((t.t1, x, t.t3, t.t4)))
    }

    implicit def lifta2F[F[_], A1, A2, A3, A4](implicit M: Monoid[Prod4[F, A1, A2, A3, A4]]): Inj[Prod4[F, A1, A2, A3, A4], F[A3]] = {
      val t = M.empty
      Inj.instance(x => Prod4[F, A1, A2, A3, A4]((t.t1, t.t2, x, t.t4)))
    }

    implicit def lifta3F[F[_], A1, A2, A3, A4](implicit M: Monoid[Prod4[F, A1, A2, A3, A4]]): Inj[Prod4[F, A1, A2, A3, A4], F[A4]] = {
      val t = M.empty
      Inj.instance(x => Prod4[F, A1, A2, A3, A4]((t.t1, t.t2, t.t3, x)))
    }

    implicit def Prod4Lens0[F[_], A1, A2, A3, A4]: Lens[Prod4[F, A1, A2, A3, A4], F[A1]] =
      Lens[Prod4[F, A1, A2, A3, A4], F[A1]](p => p.t1)(x => p =>
        Prod4[F, A1, A2, A3, A4]((x, p.t2, p.t3, p.t4)))

    implicit def Prod4Lens1[F[_], A1, A2, A3, A4]: Lens[Prod4[F, A1, A2, A3, A4], F[A2]] =
      Lens[Prod4[F, A1, A2, A3, A4], F[A2]](p => p.t2)(x => p =>
        Prod4[F, A1, A2, A3, A4]((p.t1, x, p.t3, p.t4)))

    implicit def Prod4Lens2[F[_], A1, A2, A3, A4]: Lens[Prod4[F, A1, A2, A3, A4], F[A3]] =
      Lens[Prod4[F, A1, A2, A3, A4], F[A3]](p => p.t3)(x => p =>
        Prod4[F, A1, A2, A3, A4]((p.t1, p.t2, x, p.t4)))

    implicit def Prod4Lens3[F[_], A1, A2, A3, A4]: Lens[Prod4[F, A1, A2, A3, A4], F[A4]] =
      Lens[Prod4[F, A1, A2, A3, A4], F[A4]](p => p.t4)(x => p =>
        Prod4[F, A1, A2, A3, A4]((p.t1, p.t2, p.t3, x)))

  }

  object Prod4 extends Prod4LP {

    implicit def lifta0Id[A1, A2, A3, A4](implicit M: Monoid[Prod4[Id, A1, A2, A3, A4]]): Inj[Prod4[Id, A1, A2, A3, A4], A1] =
      lifta0F[Id, A1, A2, A3, A4]

    implicit def lifta1Id[A1, A2, A3, A4](implicit M: Monoid[Prod4[Id, A1, A2, A3, A4]]): Inj[Prod4[Id, A1, A2, A3, A4], A2] =
      lifta1F[Id, A1, A2, A3, A4]

    implicit def lifta2Id[A1, A2, A3, A4](implicit M: Monoid[Prod4[Id, A1, A2, A3, A4]]): Inj[Prod4[Id, A1, A2, A3, A4], A3] =
      lifta2F[Id, A1, A2, A3, A4]

    implicit def lifta3Id[A1, A2, A3, A4](implicit M: Monoid[Prod4[Id, A1, A2, A3, A4]]): Inj[Prod4[Id, A1, A2, A3, A4], A4] =
      lifta3F[Id, A1, A2, A3, A4]

    implicit def Prod4Lens0Id[A1, A2, A3, A4]: Lens[Prod4[Id, A1, A2, A3, A4], A1] =
      Prod4Lens0[Id, A1, A2, A3, A4]

    implicit def Prod4Lens1Id[A1, A2, A3, A4]: Lens[Prod4[Id, A1, A2, A3, A4], A2] =
      Prod4Lens1[Id, A1, A2, A3, A4]

    implicit def Prod4Lens2Id[A1, A2, A3, A4]: Lens[Prod4[Id, A1, A2, A3, A4], A3] =
      Prod4Lens2[Id, A1, A2, A3, A4]

    implicit def Prod4Lens3Id[A1, A2, A3, A4]: Lens[Prod4[Id, A1, A2, A3, A4], A4] =
      Prod4Lens3[Id, A1, A2, A3, A4]

  }

  @newtype case class Cop4[F[_], A1, A2, A3, A4](run: Either[F[A1], Either[F[A2], Either[F[A3], F[A4]]]]) {
    private def mapN = new Map4C[F[A1], F[A2], F[A3], F[A4]] {}

    def map1[B](f: F[A1] => F[B]): Cop4[F, B, A2, A3, A4] =
      Cop4[F, B, A2, A3, A4](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Cop4[F, B, A2, A3, A4] =
      Cop4[F, B, A2, A3, A4](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Cop4[F, A1, B, A3, A4] =
      Cop4[F, A1, B, A3, A4](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Cop4[F, A1, B, A3, A4] =
      Cop4[F, A1, B, A3, A4](mapN.mapAt(f)(run))

    def map3[B](f: F[A3] => F[B]): Cop4[F, A1, A2, B, A4] =
      Cop4[F, A1, A2, B, A4](mapN.map3(run)(f))

    def mapAt[B](f: F[A3] => F[B])(implicit d: Dummy3): Cop4[F, A1, A2, B, A4] =
      Cop4[F, A1, A2, B, A4](mapN.mapAt(f)(run))

    def map4[B](f: F[A4] => F[B]): Cop4[F, A1, A2, A3, B] =
      Cop4[F, A1, A2, A3, B](mapN.map4(run)(f))

    def mapAt[B](f: F[A4] => F[B])(implicit d: Dummy4): Cop4[F, A1, A2, A3, B] =
      Cop4[F, A1, A2, A3, B](mapN.mapAt(f)(run))

  }

  trait Cop4LP {

    implicit def Cop4Instance[A1, A2, A3, A4]: FFunctor[Cop4[?[_], A1, A2, A3, A4]] with FTraverseCop[Cop4[?[_], A1, A2, A3, A4]] =
      new FFunctor[Cop4[?[_], A1, A2, A3, A4]] with FTraverseCop[Cop4[?[_], A1, A2, A3, A4]] {
        def map[F[_], G[_]](c: Cop4[F, A1, A2, A3, A4])(nt: F ~> G): Cop4[G, A1, A2, A3, A4] =
          Cop4[G, A1, A2, A3, A4](c.run.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_)))))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop4[F, A1, A2, A3, A4])(f: F ~> Lambda[a => A[G[a]]]): A[Cop4[G, A1, A2, A3, A4]] =
          c.run match {

            case Left(x) => Functor[A].map(f(x))(y => Cop4[G, A1, A2, A3, A4](Left(y)))

            case Right(Left(x)) => Functor[A].map(f(x))(y => Cop4[G, A1, A2, A3, A4](Right(Left(y))))

            case Right(Right(Left(x))) => Functor[A].map(f(x))(y => Cop4[G, A1, A2, A3, A4](Right(Right(Left(y)))))

            case Right(Right(Right(x))) => Functor[A].map(f(x))(y => Cop4[G, A1, A2, A3, A4](Right(Right(Right(y)))))

          }
      }

    implicit def inja0F[F[_], A1, A2, A3, A4]: Inj[Cop4[F, A1, A2, A3, A4], F[A1]] =
      Inj.instance(x => Cop4[F, A1, A2, A3, A4](Left(x)))

    implicit def inja1F[F[_], A1, A2, A3, A4]: Inj[Cop4[F, A1, A2, A3, A4], F[A2]] =
      Inj.instance(x => Cop4[F, A1, A2, A3, A4](Right(Left(x))))

    implicit def inja2F[F[_], A1, A2, A3, A4]: Inj[Cop4[F, A1, A2, A3, A4], F[A3]] =
      Inj.instance(x => Cop4[F, A1, A2, A3, A4](Right(Right(Left(x)))))

    implicit def inja3F[F[_], A1, A2, A3, A4]: Inj[Cop4[F, A1, A2, A3, A4], F[A4]] =
      Inj.instance(x => Cop4[F, A1, A2, A3, A4](Right(Right(Right(x)))))

    implicit def Cop4Optional0[F[_], A1, A2, A3, A4]: Optional[Cop4[F, A1, A2, A3, A4], F[A1]] =
      Optional[Cop4[F, A1, A2, A3, A4], F[A1]](c => c.run match {
        case Left(x) => Some(x)
        case _ => None
      })(x => _ => Cop4[F, A1, A2, A3, A4](Left(x)))

    implicit def Cop4Optional1[F[_], A1, A2, A3, A4]: Optional[Cop4[F, A1, A2, A3, A4], F[A2]] =
      Optional[Cop4[F, A1, A2, A3, A4], F[A2]](c => c.run match {
        case Right(Left(x)) => Some(x)
        case _ => None
      })(x => _ => Cop4[F, A1, A2, A3, A4](Right(Left(x))))

    implicit def Cop4Optional2[F[_], A1, A2, A3, A4]: Optional[Cop4[F, A1, A2, A3, A4], F[A3]] =
      Optional[Cop4[F, A1, A2, A3, A4], F[A3]](c => c.run match {
        case Right(Right(Left(x))) => Some(x)
        case _ => None
      })(x => _ => Cop4[F, A1, A2, A3, A4](Right(Right(Left(x)))))

    implicit def Cop4Optional3[F[_], A1, A2, A3, A4]: Optional[Cop4[F, A1, A2, A3, A4], F[A4]] =
      Optional[Cop4[F, A1, A2, A3, A4], F[A4]](c => c.run match {
        case Right(Right(Right(x))) => Some(x)
        case _ => None
      })(x => _ => Cop4[F, A1, A2, A3, A4](Right(Right(Right(x)))))

  }

  object Cop4 extends Cop4LP {

    implicit def inja0Id[A1, A2, A3, A4]: Inj[Cop4[Id, A1, A2, A3, A4], A1] =
      inja0F[Id, A1, A2, A3, A4]

    implicit def inja1Id[A1, A2, A3, A4]: Inj[Cop4[Id, A1, A2, A3, A4], A2] =
      inja1F[Id, A1, A2, A3, A4]

    implicit def inja2Id[A1, A2, A3, A4]: Inj[Cop4[Id, A1, A2, A3, A4], A3] =
      inja2F[Id, A1, A2, A3, A4]

    implicit def inja3Id[A1, A2, A3, A4]: Inj[Cop4[Id, A1, A2, A3, A4], A4] =
      inja3F[Id, A1, A2, A3, A4]

    implicit def Cop4Optional0Id[A1, A2, A3, A4]: Optional[Cop4[Id, A1, A2, A3, A4], A1] =
      Cop4Optional0[Id, A1, A2, A3, A4]

    implicit def Cop4Optional1Id[A1, A2, A3, A4]: Optional[Cop4[Id, A1, A2, A3, A4], A2] =
      Cop4Optional1[Id, A1, A2, A3, A4]

    implicit def Cop4Optional2Id[A1, A2, A3, A4]: Optional[Cop4[Id, A1, A2, A3, A4], A3] =
      Cop4Optional2[Id, A1, A2, A3, A4]

    implicit def Cop4Optional3Id[A1, A2, A3, A4]: Optional[Cop4[Id, A1, A2, A3, A4], A4] =
      Cop4Optional3[Id, A1, A2, A3, A4]

  }
}
