package andxor.types

import andxor._
import scalaz.{~>, Applicative, Functor, Lens, Monoid, PLens, PlusEmpty, StoreT}
import scalaz.Id.Id
import scalaz.Isomorphism.IsoSet

trait Types2 {
  @newtype case class Prod2[F[_], A1, A2](run: (F[A1], F[A2])) { self =>
    def t1: F[A1] = run._1
    def t2: F[A2] = run._2

    private def mapN = new Map2P[F[A1], F[A2]] {}

    def map1[B](f: F[A1] => F[B]): Prod2[F, B, A2] =
      Prod2[F, B, A2](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Prod2[F, B, A2] =
      Prod2[F, B, A2](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Prod2[F, A1, B] =
      Prod2[F, A1, B](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Prod2[F, A1, B] =
      Prod2[F, A1, B](mapN.mapAt(f)(run))

  }

  trait Prod2LP {

    implicit def Prod2Instance[A1, A2]: FFunctor[Prod2[?[_], A1, A2]] with FTraverseProd[Prod2[?[_], A1, A2]] =
      new FFunctor[Prod2[?[_], A1, A2]] with FTraverseProd[Prod2[?[_], A1, A2]] {
        def map[F[_], G[_]](p: Prod2[F, A1, A2])(nt: F ~> G): Prod2[G, A1, A2] =
          Prod2[G, A1, A2]((nt(p.t1), nt(p.t2)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod2[F, A1, A2])(f: F ~> Lambda[a => A[G[a]]]): A[Prod2[G, A1, A2]] =
          Applicative[A].ap(f(p.t2))(Applicative[A].map(f(p.t1))((i0: G[A1]) => (i1: G[A2]) => Prod2[G, A1, A2]((i0, i1))))
      }

    implicit def Prod2FoldMap[A1, A2]: FoldMap[Prod2[?[_], A1, A2], Cop2[?[_], A1, A2]] =
      new FoldMap[Prod2[?[_], A1, A2], Cop2[?[_], A1, A2]] {
        def emptyProd[F[_]](implicit PE: PlusEmpty[F]): Prod2[F, A1, A2] =
          Prod2[F, A1, A2]((PE.empty[A1], PE.empty[A2]))

        def unconsAll[F[_], G[_]](p: Prod2[F, A1, A2])(implicit U: Uncons[F, G]): (List[Cop2[G, A1, A2]], Prod2[F, A1, A2]) = {
          val (h1, t1) = U(p.t1)
          val (h2, t2) = U(p.t2)
          (
            List(h1.map(Inj[Cop2[G, A1, A2], G[A1]].apply(_)), h2.map(Inj[Cop2[G, A1, A2], G[A2]].apply(_))).flatten,
            Prod2[F, A1, A2]((t1, t2)))
        }

        def unconsOne[F[_], G[_]](p: Prod2[F, A1, A2], c: Cop2[G, A1, A2])(implicit U: Uncons[F, G]): (Option[Cop2[G, A1, A2]], Prod2[F, A1, A2]) =
          c.run match {

            case Left(_) =>
              val (h, t) = U(p.t1)
              (h.map(v => Cop2[G, A1, A2](Left(v))), Prod2[F, A1, A2]((t, p.t2)))

            case Right(_) =>
              val (h, t) = U(p.t2)
              (h.map(v => Cop2[G, A1, A2](Right(v))), Prod2[F, A1, A2]((p.t1, t)))

          }
      }

    def Prod2TupleIso[F[_], A1, A2]: IsoSet[Prod2[F, A1, A2], (F[A1], F[A2])] =
      IsoSet((_: Prod2[F, A1, A2]).run, Prod2[F, A1, A2](_: (F[A1], F[A2])))

    implicit def Prod2Monoid[F[_], A1, A2](implicit M: Monoid[(F[A1], F[A2])]): Monoid[Prod2[F, A1, A2]] =
      Monoid.fromIso(Prod2TupleIso[F, A1, A2])(M)

    implicit def lifta0F[F[_], A1, A2](implicit M: Monoid[Prod2[F, A1, A2]]): Inj[Prod2[F, A1, A2], F[A1]] = {
      val t = M.zero
      Inj.instance(x => Prod2[F, A1, A2]((x, t.t2)))
    }

    implicit def lifta1F[F[_], A1, A2](implicit M: Monoid[Prod2[F, A1, A2]]): Inj[Prod2[F, A1, A2], F[A2]] = {
      val t = M.zero
      Inj.instance(x => Prod2[F, A1, A2]((t.t1, x)))
    }

    implicit def Prod2Lens0[F[_], A1, A2]: Lens[Prod2[F, A1, A2], F[A1]] =
      Lens(p => StoreT.store[F[A1], Prod2[F, A1, A2]](p.t1)(x =>
        Prod2[F, A1, A2]((x, p.t2))))

    implicit def Prod2Lens1[F[_], A1, A2]: Lens[Prod2[F, A1, A2], F[A2]] =
      Lens(p => StoreT.store[F[A2], Prod2[F, A1, A2]](p.t2)(x =>
        Prod2[F, A1, A2]((p.t1, x))))

  }

  object Prod2 extends Prod2LP {

    implicit def lifta0Id[A1, A2](implicit M: Monoid[Prod2[Id, A1, A2]]): Inj[Prod2[Id, A1, A2], A1] =
      lifta0F[Id, A1, A2]

    implicit def lifta1Id[A1, A2](implicit M: Monoid[Prod2[Id, A1, A2]]): Inj[Prod2[Id, A1, A2], A2] =
      lifta1F[Id, A1, A2]

    implicit def Prod2Lens0Id[A1, A2]: Lens[Prod2[Id, A1, A2], A1] =
      Prod2Lens0[Id, A1, A2]

    implicit def Prod2Lens1Id[A1, A2]: Lens[Prod2[Id, A1, A2], A2] =
      Prod2Lens1[Id, A1, A2]

  }

  @newtype case class Cop2[F[_], A1, A2](run: Either[F[A1], F[A2]]) {
    private def mapN = new Map2C[F[A1], F[A2]] {}

    def map1[B](f: F[A1] => F[B]): Cop2[F, B, A2] =
      Cop2[F, B, A2](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Cop2[F, B, A2] =
      Cop2[F, B, A2](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Cop2[F, A1, B] =
      Cop2[F, A1, B](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Cop2[F, A1, B] =
      Cop2[F, A1, B](mapN.mapAt(f)(run))

  }

  trait Cop2LP {

    implicit def Cop2Instance[A1, A2]: FFunctor[Cop2[?[_], A1, A2]] with FTraverseCop[Cop2[?[_], A1, A2]] =
      new FFunctor[Cop2[?[_], A1, A2]] with FTraverseCop[Cop2[?[_], A1, A2]] {
        def map[F[_], G[_]](c: Cop2[F, A1, A2])(nt: F ~> G): Cop2[G, A1, A2] =
          Cop2[G, A1, A2](c.run.bimap(nt(_), nt(_)))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop2[F, A1, A2])(f: F ~> Lambda[a => A[G[a]]]): A[Cop2[G, A1, A2]] =
          c.run match {

            case Left(x) => Functor[A].map(f(x))(y => Cop2[G, A1, A2](Left(y)))

            case Right(x) => Functor[A].map(f(x))(y => Cop2[G, A1, A2](Right(y)))

          }
      }

    implicit def inja0F[F[_], A1, A2]: Inj[Cop2[F, A1, A2], F[A1]] =
      Inj.instance(x => Cop2[F, A1, A2](Left(x)))

    implicit def inja1F[F[_], A1, A2]: Inj[Cop2[F, A1, A2], F[A2]] =
      Inj.instance(x => Cop2[F, A1, A2](Right(x)))

    implicit def Cop2PLens0[F[_], A1, A2]: PLens[Cop2[F, A1, A2], F[A1]] =
      PLens(c => c.run match {
        case Left(x) => Some(StoreT.store[F[A1], Cop2[F, A1, A2]](x)(y => Cop2[F, A1, A2](Left(y))))
        case _ => None
      })

    implicit def Cop2PLens1[F[_], A1, A2]: PLens[Cop2[F, A1, A2], F[A2]] =
      PLens(c => c.run match {
        case Right(x) => Some(StoreT.store[F[A2], Cop2[F, A1, A2]](x)(y => Cop2[F, A1, A2](Right(y))))
        case _ => None
      })

  }

  object Cop2 extends Cop2LP {

    implicit def inja0Id[A1, A2]: Inj[Cop2[Id, A1, A2], A1] =
      inja0F[Id, A1, A2]

    implicit def inja1Id[A1, A2]: Inj[Cop2[Id, A1, A2], A2] =
      inja1F[Id, A1, A2]

    implicit def Cop2PLens0Id[A1, A2]: PLens[Cop2[Id, A1, A2], A1] =
      Cop2PLens0[Id, A1, A2]

    implicit def Cop2PLens1Id[A1, A2]: PLens[Cop2[Id, A1, A2], A2] =
      Cop2PLens1[Id, A1, A2]

  }
}
