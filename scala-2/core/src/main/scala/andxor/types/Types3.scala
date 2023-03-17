package andxor.types

import andxor._
import monocle.{Lens, Optional}
import cats.{~>, Applicative, Functor, Id, Monoid, MonoidK}
import cats.syntax.either._
import cats.syntax.invariant._
import io.estatico.newtype.macros.newtype
import monocle.Iso

trait Types3 {
  final type Prod3[F[_], A1, A2, A3] = Types3.Prod3[F, A1, A2, A3]
  final val Prod3: Types3.Prod3.type = Types3.Prod3
  final type Cop3[F[_], A1, A2, A3] = Types3.Cop3[F, A1, A2, A3]
  final val Cop3: Types3.Cop3.type = Types3.Cop3
}

object Types3 {
  @newtype case class Prod3[F[_], A1, A2, A3](run: (F[A1], F[A2], F[A3])) { self =>
    def t1: F[A1] = run._1
    def t2: F[A2] = run._2
    def t3: F[A3] = run._3

    private def mapN = new Map3P[F[A1], F[A2], F[A3]] {}

    def map1[B](f: F[A1] => F[B]): Prod3[F, B, A2, A3] =
      Prod3[F, B, A2, A3](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Prod3[F, B, A2, A3] =
      Prod3[F, B, A2, A3](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Prod3[F, A1, B, A3] =
      Prod3[F, A1, B, A3](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Prod3[F, A1, B, A3] =
      Prod3[F, A1, B, A3](mapN.mapAt(f)(run))

    def map3[B](f: F[A3] => F[B]): Prod3[F, A1, A2, B] =
      Prod3[F, A1, A2, B](mapN.map3(run)(f))

    def mapAt[B](f: F[A3] => F[B])(implicit d: Dummy3): Prod3[F, A1, A2, B] =
      Prod3[F, A1, A2, B](mapN.mapAt(f)(run))

  }

  trait Prod3LP {

    implicit def Prod3Instance[A1, A2, A3]: FFunctor[Prod3[*[_], A1, A2, A3]] with FTraverseProd[Prod3[*[_], A1, A2, A3]] =
      new FFunctor[Prod3[*[_], A1, A2, A3]] with FTraverseProd[Prod3[*[_], A1, A2, A3]] {
        def map[F[_], G[_]](p: Prod3[F, A1, A2, A3])(nt: F ~> G): Prod3[G, A1, A2, A3] =
          Prod3[G, A1, A2, A3]((nt(p.t1), nt(p.t2), nt(p.t3)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod3[F, A1, A2, A3])(f: F ~> Lambda[a => A[G[a]]]): A[Prod3[G, A1, A2, A3]] =
          Applicative[A].ap(Applicative[A].ap(Applicative[A].map(f(p.t1))((i0: G[A1]) => (i1: G[A2]) => (i2: G[A3]) => Prod3[G, A1, A2, A3]((i0, i1, i2))))(f(p.t2)))(f(p.t3))
      }

    implicit def Prod3FoldMap[A1, A2, A3]: FoldMap[Prod3[*[_], A1, A2, A3], Cop3[*[_], A1, A2, A3]] =
      new FoldMap[Prod3[*[_], A1, A2, A3], Cop3[*[_], A1, A2, A3]] {
        def emptyProd[F[_]](implicit PE: MonoidK[F]): Prod3[F, A1, A2, A3] =
          Prod3[F, A1, A2, A3]((PE.empty[A1], PE.empty[A2], PE.empty[A3]))

        def unconsAll[F[_], G[_]](p: Prod3[F, A1, A2, A3])(implicit U: Uncons[F, G]): (List[Cop3[G, A1, A2, A3]], Prod3[F, A1, A2, A3]) = {
          val (h1, t1) = U(p.t1)
          val (h2, t2) = U(p.t2)
          val (h3, t3) = U(p.t3)
          (
            List(h1.map(Inj[Cop3[G, A1, A2, A3], G[A1]].apply(_)), h2.map(Inj[Cop3[G, A1, A2, A3], G[A2]].apply(_)), h3.map(Inj[Cop3[G, A1, A2, A3], G[A3]].apply(_))).flatten,
            Prod3[F, A1, A2, A3]((t1, t2, t3)))
        }

        def unconsOne[F[_], G[_]](p: Prod3[F, A1, A2, A3], c: Cop3[G, A1, A2, A3])(implicit U: Uncons[F, G]): (Option[Cop3[G, A1, A2, A3]], Prod3[F, A1, A2, A3]) =
          c.run match {

            case Left(_) =>
              val (h, t) = U(p.t1)
              (h.map(v => Cop3[G, A1, A2, A3](Left(v))), Prod3[F, A1, A2, A3]((t, p.t2, p.t3)))

            case Right(Left(_)) =>
              val (h, t) = U(p.t2)
              (h.map(v => Cop3[G, A1, A2, A3](Right(Left(v)))), Prod3[F, A1, A2, A3]((p.t1, t, p.t3)))

            case Right(Right(_)) =>
              val (h, t) = U(p.t3)
              (h.map(v => Cop3[G, A1, A2, A3](Right(Right(v)))), Prod3[F, A1, A2, A3]((p.t1, p.t2, t)))

          }
      }

    def Prod3TupleIso[F[_], A1, A2, A3]: Iso[Prod3[F, A1, A2, A3], (F[A1], F[A2], F[A3])] =
      Iso((_: Prod3[F, A1, A2, A3]).run)(Prod3[F, A1, A2, A3](_: (F[A1], F[A2], F[A3])))

    implicit def Prod3Monoid[F[_], A1, A2, A3](implicit M: Monoid[(F[A1], F[A2], F[A3])]): Monoid[Prod3[F, A1, A2, A3]] = {
      val iso = Prod3TupleIso[F, A1, A2, A3]
      M.imap(iso.reverseGet)(iso.get)
    }

    implicit def lifta0F[F[_], A1, A2, A3](implicit M: Monoid[Prod3[F, A1, A2, A3]]): Inj[Prod3[F, A1, A2, A3], F[A1]] = {
      val t = M.empty
      Inj.instance(x => Prod3[F, A1, A2, A3]((x, t.t2, t.t3)))
    }

    implicit def lifta1F[F[_], A1, A2, A3](implicit M: Monoid[Prod3[F, A1, A2, A3]]): Inj[Prod3[F, A1, A2, A3], F[A2]] = {
      val t = M.empty
      Inj.instance(x => Prod3[F, A1, A2, A3]((t.t1, x, t.t3)))
    }

    implicit def lifta2F[F[_], A1, A2, A3](implicit M: Monoid[Prod3[F, A1, A2, A3]]): Inj[Prod3[F, A1, A2, A3], F[A3]] = {
      val t = M.empty
      Inj.instance(x => Prod3[F, A1, A2, A3]((t.t1, t.t2, x)))
    }

    implicit def Prod3Lens0[F[_], A1, A2, A3]: Lens[Prod3[F, A1, A2, A3], F[A1]] =
      Lens[Prod3[F, A1, A2, A3], F[A1]](p => p.t1)(x => p =>
        Prod3[F, A1, A2, A3]((x, p.t2, p.t3)))

    implicit def Prod3Lens1[F[_], A1, A2, A3]: Lens[Prod3[F, A1, A2, A3], F[A2]] =
      Lens[Prod3[F, A1, A2, A3], F[A2]](p => p.t2)(x => p =>
        Prod3[F, A1, A2, A3]((p.t1, x, p.t3)))

    implicit def Prod3Lens2[F[_], A1, A2, A3]: Lens[Prod3[F, A1, A2, A3], F[A3]] =
      Lens[Prod3[F, A1, A2, A3], F[A3]](p => p.t3)(x => p =>
        Prod3[F, A1, A2, A3]((p.t1, p.t2, x)))

  }

  object Prod3 extends Prod3LP {

    implicit def lifta0Id[A1, A2, A3](implicit M: Monoid[Prod3[Id, A1, A2, A3]]): Inj[Prod3[Id, A1, A2, A3], A1] =
      lifta0F[Id, A1, A2, A3]

    implicit def lifta1Id[A1, A2, A3](implicit M: Monoid[Prod3[Id, A1, A2, A3]]): Inj[Prod3[Id, A1, A2, A3], A2] =
      lifta1F[Id, A1, A2, A3]

    implicit def lifta2Id[A1, A2, A3](implicit M: Monoid[Prod3[Id, A1, A2, A3]]): Inj[Prod3[Id, A1, A2, A3], A3] =
      lifta2F[Id, A1, A2, A3]

    implicit def Prod3Lens0Id[A1, A2, A3]: Lens[Prod3[Id, A1, A2, A3], A1] =
      Prod3Lens0[Id, A1, A2, A3]

    implicit def Prod3Lens1Id[A1, A2, A3]: Lens[Prod3[Id, A1, A2, A3], A2] =
      Prod3Lens1[Id, A1, A2, A3]

    implicit def Prod3Lens2Id[A1, A2, A3]: Lens[Prod3[Id, A1, A2, A3], A3] =
      Prod3Lens2[Id, A1, A2, A3]

  }

  @newtype case class Cop3[F[_], A1, A2, A3](run: Either[F[A1], Either[F[A2], F[A3]]]) {
    private def mapN = new Map3C[F[A1], F[A2], F[A3]] {}

    def map1[B](f: F[A1] => F[B]): Cop3[F, B, A2, A3] =
      Cop3[F, B, A2, A3](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Cop3[F, B, A2, A3] =
      Cop3[F, B, A2, A3](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Cop3[F, A1, B, A3] =
      Cop3[F, A1, B, A3](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Cop3[F, A1, B, A3] =
      Cop3[F, A1, B, A3](mapN.mapAt(f)(run))

    def map3[B](f: F[A3] => F[B]): Cop3[F, A1, A2, B] =
      Cop3[F, A1, A2, B](mapN.map3(run)(f))

    def mapAt[B](f: F[A3] => F[B])(implicit d: Dummy3): Cop3[F, A1, A2, B] =
      Cop3[F, A1, A2, B](mapN.mapAt(f)(run))

  }

  trait Cop3LP {

    implicit def Cop3Instance[A1, A2, A3]: FFunctor[Cop3[*[_], A1, A2, A3]] with FTraverseCop[Cop3[*[_], A1, A2, A3]] =
      new FFunctor[Cop3[*[_], A1, A2, A3]] with FTraverseCop[Cop3[*[_], A1, A2, A3]] {
        def map[F[_], G[_]](c: Cop3[F, A1, A2, A3])(nt: F ~> G): Cop3[G, A1, A2, A3] =
          Cop3[G, A1, A2, A3](c.run.bimap(nt(_), _.bimap(nt(_), nt(_))))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop3[F, A1, A2, A3])(f: F ~> Lambda[a => A[G[a]]]): A[Cop3[G, A1, A2, A3]] =
          c.run match {

            case Left(x) => Functor[A].map(f(x))(y => Cop3[G, A1, A2, A3](Left(y)))

            case Right(Left(x)) => Functor[A].map(f(x))(y => Cop3[G, A1, A2, A3](Right(Left(y))))

            case Right(Right(x)) => Functor[A].map(f(x))(y => Cop3[G, A1, A2, A3](Right(Right(y))))

          }
      }

    implicit def inja0F[F[_], A1, A2, A3]: Inj[Cop3[F, A1, A2, A3], F[A1]] =
      Inj.instance(x => Cop3[F, A1, A2, A3](Left(x)))

    implicit def inja1F[F[_], A1, A2, A3]: Inj[Cop3[F, A1, A2, A3], F[A2]] =
      Inj.instance(x => Cop3[F, A1, A2, A3](Right(Left(x))))

    implicit def inja2F[F[_], A1, A2, A3]: Inj[Cop3[F, A1, A2, A3], F[A3]] =
      Inj.instance(x => Cop3[F, A1, A2, A3](Right(Right(x))))

    implicit def Cop3Optional0[F[_], A1, A2, A3]: Optional[Cop3[F, A1, A2, A3], F[A1]] =
      Optional[Cop3[F, A1, A2, A3], F[A1]](c => c.run match {
        case Left(x) => Some(x)
        case _ => None
      })(x => _ => Cop3[F, A1, A2, A3](Left(x)))

    implicit def Cop3Optional1[F[_], A1, A2, A3]: Optional[Cop3[F, A1, A2, A3], F[A2]] =
      Optional[Cop3[F, A1, A2, A3], F[A2]](c => c.run match {
        case Right(Left(x)) => Some(x)
        case _ => None
      })(x => _ => Cop3[F, A1, A2, A3](Right(Left(x))))

    implicit def Cop3Optional2[F[_], A1, A2, A3]: Optional[Cop3[F, A1, A2, A3], F[A3]] =
      Optional[Cop3[F, A1, A2, A3], F[A3]](c => c.run match {
        case Right(Right(x)) => Some(x)
        case _ => None
      })(x => _ => Cop3[F, A1, A2, A3](Right(Right(x))))

  }

  object Cop3 extends Cop3LP {

    implicit def inja0Id[A1, A2, A3]: Inj[Cop3[Id, A1, A2, A3], A1] =
      inja0F[Id, A1, A2, A3]

    implicit def inja1Id[A1, A2, A3]: Inj[Cop3[Id, A1, A2, A3], A2] =
      inja1F[Id, A1, A2, A3]

    implicit def inja2Id[A1, A2, A3]: Inj[Cop3[Id, A1, A2, A3], A3] =
      inja2F[Id, A1, A2, A3]

    implicit def Cop3Optional0Id[A1, A2, A3]: Optional[Cop3[Id, A1, A2, A3], A1] =
      Cop3Optional0[Id, A1, A2, A3]

    implicit def Cop3Optional1Id[A1, A2, A3]: Optional[Cop3[Id, A1, A2, A3], A2] =
      Cop3Optional1[Id, A1, A2, A3]

    implicit def Cop3Optional2Id[A1, A2, A3]: Optional[Cop3[Id, A1, A2, A3], A3] =
      Cop3Optional2[Id, A1, A2, A3]

  }
}
