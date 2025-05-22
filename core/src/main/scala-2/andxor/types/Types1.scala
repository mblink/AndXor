package andxor.types

import andxor._

import monocle.{Lens, Optional}
import cats.{~>, Applicative, Functor, Id, Monoid, MonoidK}

import cats.syntax.invariant._
import io.estatico.newtype.macros.newtype
import monocle.Iso

trait Types1 {
  final type Prod1[F[_], A1] = Types1.Prod1[F, A1]
  final val Prod1: Types1.Prod1.type = Types1.Prod1
  final type Cop1[F[_], A1] = Types1.Cop1[F, A1]
  final val Cop1: Types1.Cop1.type = Types1.Cop1
}

object Types1 {
  @newtype case class Prod1[F[_], A1](run: F[A1]) { self =>
    def t1: F[A1] = run

  }

  trait Prod1LP {
    implicit def conv[F[_], A](a: F[A]): Prod1[F, A] = Prod1[F, A](a)

    implicit def Prod1Instance[A1]: FFunctor[Prod1[*[_], A1]] with FTraverseProd[Prod1[*[_], A1]] =
      new FFunctor[Prod1[*[_], A1]] with FTraverseProd[Prod1[*[_], A1]] {
        def map[F[_], G[_]](p: Prod1[F, A1])(nt: F ~> G): Prod1[G, A1] =
          Prod1[G, A1](nt(p.run))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod1[F, A1])(f: F ~> Lambda[a => A[G[a]]]): A[Prod1[G, A1]] =
          Applicative[A].map(f(p.run))((i0: G[A1]) => Prod1[G, A1](i0))
      }

    implicit def Prod1FoldMap[A1]: FoldMap[Prod1[*[_], A1], Cop1[*[_], A1]] =
      new FoldMap[Prod1[*[_], A1], Cop1[*[_], A1]] {
        def emptyProd[F[_]](implicit PE: MonoidK[F]): Prod1[F, A1] =
          Prod1[F, A1](PE.empty[A1])

        def unconsAll[F[_], G[_]](p: Prod1[F, A1])(implicit U: Uncons[F, G]): (List[Cop1[G, A1]], Prod1[F, A1]) = {
          val (h1, t1) = U(p.run)
          (
            List(h1.map(Inj[Cop1[G, A1], G[A1]].apply(_))).flatten,
            Prod1[F, A1](t1))
        }

        def unconsOne[F[_], G[_]](p: Prod1[F, A1], c: Cop1[G, A1])(implicit U: Uncons[F, G]): (Option[Cop1[G, A1]], Prod1[F, A1]) =
          c.run match {

            case _ =>
              val (h, t) = U(p.run)
              (h.map(v => Cop1[G, A1](v)), Prod1[F, A1](t))

          }
      }

    def Prod1TupleIso[F[_], A1]: Iso[Prod1[F, A1], F[A1]] =
      Iso((_: Prod1[F, A1]).run)(Prod1[F, A1](_: F[A1]))

    implicit def Prod1Monoid[F[_], A1](implicit M: Monoid[F[A1]]): Monoid[Prod1[F, A1]] = {
      val iso = Prod1TupleIso[F, A1]
      M.imap(iso.reverseGet)(iso.get)
    }

    implicit def lifta0F[F[_], A1]: Inj[Prod1[F, A1], F[A1]] = {
      Inj.instance(x => Prod1[F, A1](x))
    }

    implicit def injProdToVecCop[F[_], A1]: Inj[Vector[Cop1[F, A1]], Prod1[F, A1]] =
      Inj.instance(p => Vector(
        Cop1[F, A1](p.t1)))

    implicit def Prod1Lens0[F[_], A1]: Lens[Prod1[F, A1], F[A1]] =
      Lens[Prod1[F, A1], F[A1]](p => p.run)(x => _ =>
        Prod1[F, A1](x))

  }

  object Prod1 extends Prod1LP {
    implicit def convId[A](a: A): Prod1[Id, A] = Prod1[Id, A](a)

    implicit def lifta0Id[A1]: Inj[Prod1[Id, A1], A1] =
      lifta0F[Id, A1]

    implicit def Prod1Lens0Id[A1]: Lens[Prod1[Id, A1], A1] =
      Prod1Lens0[Id, A1]

  }

  @newtype case class Cop1[F[_], A1](run: F[A1])

  trait Cop1LP {
    implicit def conv[F[_], A](a: F[A]): Cop1[F, A] = Cop1[F, A](a)

    implicit def Cop1Instance[A1]: FFunctor[Cop1[*[_], A1]] with FTraverseCop[Cop1[*[_], A1]] =
      new FFunctor[Cop1[*[_], A1]] with FTraverseCop[Cop1[*[_], A1]] {
        def map[F[_], G[_]](c: Cop1[F, A1])(nt: F ~> G): Cop1[G, A1] =
          Cop1[G, A1](nt(c.run))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop1[F, A1])(f: F ~> Lambda[a => A[G[a]]]): A[Cop1[G, A1]] =
          c.run match {

            case x => Functor[A].map(f(x))(y => Cop1[G, A1](y))

          }
      }

    implicit def inja0F[F[_], A1]: Inj[Cop1[F, A1], F[A1]] =
      Inj.instance(x => Cop1[F, A1](x))

    implicit def injCopToProd[F[_], A1]: Inj[Prod1[F, A1], Cop1[F, A1]] =
      Inj.instance(_.run match {
        case x => Prod1.lifta0F[F, A1].apply(x)
      })

    implicit def Cop1Optional0[F[_], A1]: Optional[Cop1[F, A1], F[A1]] =
      Optional[Cop1[F, A1], F[A1]](c => c.run match {
        case x => Some(x)

      })(x => _ => Cop1[F, A1](x))

  }

  object Cop1 extends Cop1LP {
    implicit def convId[A](a: A): Cop1[Id, A] = Cop1[Id, A](a)

    implicit def inja0Id[A1]: Inj[Cop1[Id, A1], A1] =
      inja0F[Id, A1]

    implicit def Cop1Optional0Id[A1]: Optional[Cop1[Id, A1], A1] =
      Cop1Optional0[Id, A1]

  }
}
