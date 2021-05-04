package andxor.types

import andxor._
import monocle.{Lens, Optional}
import cats.{~>, Applicative, Functor, Id, Monoid, MonoidK}
import cats.syntax.either._
import cats.syntax.invariant._
import monocle.Iso

trait Types5 {
  @newtype case class Prod5[F[_], A1, A2, A3, A4, A5](run: (F[A1], F[A2], F[A3], F[A4], F[A5])) { self =>
    def t1: F[A1] = run._1
    def t2: F[A2] = run._2
    def t3: F[A3] = run._3
    def t4: F[A4] = run._4
    def t5: F[A5] = run._5

    private def mapN = new Map5P[F[A1], F[A2], F[A3], F[A4], F[A5]] {}

    def map1[B](f: F[A1] => F[B]): Prod5[F, B, A2, A3, A4, A5] =
      Prod5[F, B, A2, A3, A4, A5](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Prod5[F, B, A2, A3, A4, A5] =
      Prod5[F, B, A2, A3, A4, A5](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Prod5[F, A1, B, A3, A4, A5] =
      Prod5[F, A1, B, A3, A4, A5](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Prod5[F, A1, B, A3, A4, A5] =
      Prod5[F, A1, B, A3, A4, A5](mapN.mapAt(f)(run))

    def map3[B](f: F[A3] => F[B]): Prod5[F, A1, A2, B, A4, A5] =
      Prod5[F, A1, A2, B, A4, A5](mapN.map3(run)(f))

    def mapAt[B](f: F[A3] => F[B])(implicit d: Dummy3): Prod5[F, A1, A2, B, A4, A5] =
      Prod5[F, A1, A2, B, A4, A5](mapN.mapAt(f)(run))

    def map4[B](f: F[A4] => F[B]): Prod5[F, A1, A2, A3, B, A5] =
      Prod5[F, A1, A2, A3, B, A5](mapN.map4(run)(f))

    def mapAt[B](f: F[A4] => F[B])(implicit d: Dummy4): Prod5[F, A1, A2, A3, B, A5] =
      Prod5[F, A1, A2, A3, B, A5](mapN.mapAt(f)(run))

    def map5[B](f: F[A5] => F[B]): Prod5[F, A1, A2, A3, A4, B] =
      Prod5[F, A1, A2, A3, A4, B](mapN.map5(run)(f))

    def mapAt[B](f: F[A5] => F[B])(implicit d: Dummy5): Prod5[F, A1, A2, A3, A4, B] =
      Prod5[F, A1, A2, A3, A4, B](mapN.mapAt(f)(run))

  }

  trait Prod5LP {

    implicit def Prod5Instance[A1, A2, A3, A4, A5]: FFunctor[Prod5[*[_], A1, A2, A3, A4, A5]] with FTraverseProd[Prod5[*[_], A1, A2, A3, A4, A5]] =
      new FFunctor[Prod5[*[_], A1, A2, A3, A4, A5]] with FTraverseProd[Prod5[*[_], A1, A2, A3, A4, A5]] {
        def map[F[_], G[_]](p: Prod5[F, A1, A2, A3, A4, A5])(nt: F ~> G): Prod5[G, A1, A2, A3, A4, A5] =
          Prod5[G, A1, A2, A3, A4, A5]((nt(p.t1), nt(p.t2), nt(p.t3), nt(p.t4), nt(p.t5)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod5[F, A1, A2, A3, A4, A5])(f: F ~> Lambda[a => A[G[a]]]): A[Prod5[G, A1, A2, A3, A4, A5]] =
          Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].ap(Applicative[A].map(f(p.t1))((i0: G[A1]) => (i1: G[A2]) => (i2: G[A3]) => (i3: G[A4]) => (i4: G[A5]) => Prod5[G, A1, A2, A3, A4, A5]((i0, i1, i2, i3, i4))))(f(p.t2)))(f(p.t3)))(f(p.t4)))(f(p.t5))
      }

    implicit def Prod5FoldMap[A1, A2, A3, A4, A5]: FoldMap[Prod5[*[_], A1, A2, A3, A4, A5], Cop5[*[_], A1, A2, A3, A4, A5]] =
      new FoldMap[Prod5[*[_], A1, A2, A3, A4, A5], Cop5[*[_], A1, A2, A3, A4, A5]] {
        def emptyProd[F[_]](implicit PE: MonoidK[F]): Prod5[F, A1, A2, A3, A4, A5] =
          Prod5[F, A1, A2, A3, A4, A5]((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5]))

        def unconsAll[F[_], G[_]](p: Prod5[F, A1, A2, A3, A4, A5])(implicit U: Uncons[F, G]): (List[Cop5[G, A1, A2, A3, A4, A5]], Prod5[F, A1, A2, A3, A4, A5]) = {
          val (h1, t1) = U(p.t1)
          val (h2, t2) = U(p.t2)
          val (h3, t3) = U(p.t3)
          val (h4, t4) = U(p.t4)
          val (h5, t5) = U(p.t5)
          (
            List(h1.map(Inj[Cop5[G, A1, A2, A3, A4, A5], G[A1]].apply(_)), h2.map(Inj[Cop5[G, A1, A2, A3, A4, A5], G[A2]].apply(_)), h3.map(Inj[Cop5[G, A1, A2, A3, A4, A5], G[A3]].apply(_)), h4.map(Inj[Cop5[G, A1, A2, A3, A4, A5], G[A4]].apply(_)), h5.map(Inj[Cop5[G, A1, A2, A3, A4, A5], G[A5]].apply(_))).flatten,
            Prod5[F, A1, A2, A3, A4, A5]((t1, t2, t3, t4, t5)))
        }

        def unconsOne[F[_], G[_]](p: Prod5[F, A1, A2, A3, A4, A5], c: Cop5[G, A1, A2, A3, A4, A5])(implicit U: Uncons[F, G]): (Option[Cop5[G, A1, A2, A3, A4, A5]], Prod5[F, A1, A2, A3, A4, A5]) =
          c.run match {

            case Left(_) =>
              val (h, t) = U(p.t1)
              (h.map(v => Cop5[G, A1, A2, A3, A4, A5](Left(v))), Prod5[F, A1, A2, A3, A4, A5]((t, p.t2, p.t3, p.t4, p.t5)))

            case Right(Left(_)) =>
              val (h, t) = U(p.t2)
              (h.map(v => Cop5[G, A1, A2, A3, A4, A5](Right(Left(v)))), Prod5[F, A1, A2, A3, A4, A5]((p.t1, t, p.t3, p.t4, p.t5)))

            case Right(Right(Left(_))) =>
              val (h, t) = U(p.t3)
              (h.map(v => Cop5[G, A1, A2, A3, A4, A5](Right(Right(Left(v))))), Prod5[F, A1, A2, A3, A4, A5]((p.t1, p.t2, t, p.t4, p.t5)))

            case Right(Right(Right(Left(_)))) =>
              val (h, t) = U(p.t4)
              (h.map(v => Cop5[G, A1, A2, A3, A4, A5](Right(Right(Right(Left(v)))))), Prod5[F, A1, A2, A3, A4, A5]((p.t1, p.t2, p.t3, t, p.t5)))

            case Right(Right(Right(Right(_)))) =>
              val (h, t) = U(p.t5)
              (h.map(v => Cop5[G, A1, A2, A3, A4, A5](Right(Right(Right(Right(v)))))), Prod5[F, A1, A2, A3, A4, A5]((p.t1, p.t2, p.t3, p.t4, t)))

          }
      }

    def Prod5TupleIso[F[_], A1, A2, A3, A4, A5]: Iso[Prod5[F, A1, A2, A3, A4, A5], (F[A1], F[A2], F[A3], F[A4], F[A5])] =
      Iso((_: Prod5[F, A1, A2, A3, A4, A5]).run)(Prod5[F, A1, A2, A3, A4, A5](_: (F[A1], F[A2], F[A3], F[A4], F[A5])))

    implicit def Prod5Monoid[F[_], A1, A2, A3, A4, A5](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5])]): Monoid[Prod5[F, A1, A2, A3, A4, A5]] = {
      val iso = Prod5TupleIso[F, A1, A2, A3, A4, A5]
      M.imap(iso.reverseGet)(iso.get)
    }

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[F, A1, A2, A3, A4, A5]]): Inj[Prod5[F, A1, A2, A3, A4, A5], F[A1]] = {
      val t = M.empty
      Inj.instance(x => Prod5[F, A1, A2, A3, A4, A5]((x, t.t2, t.t3, t.t4, t.t5)))
    }

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[F, A1, A2, A3, A4, A5]]): Inj[Prod5[F, A1, A2, A3, A4, A5], F[A2]] = {
      val t = M.empty
      Inj.instance(x => Prod5[F, A1, A2, A3, A4, A5]((t.t1, x, t.t3, t.t4, t.t5)))
    }

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[F, A1, A2, A3, A4, A5]]): Inj[Prod5[F, A1, A2, A3, A4, A5], F[A3]] = {
      val t = M.empty
      Inj.instance(x => Prod5[F, A1, A2, A3, A4, A5]((t.t1, t.t2, x, t.t4, t.t5)))
    }

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[F, A1, A2, A3, A4, A5]]): Inj[Prod5[F, A1, A2, A3, A4, A5], F[A4]] = {
      val t = M.empty
      Inj.instance(x => Prod5[F, A1, A2, A3, A4, A5]((t.t1, t.t2, t.t3, x, t.t5)))
    }

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[F, A1, A2, A3, A4, A5]]): Inj[Prod5[F, A1, A2, A3, A4, A5], F[A5]] = {
      val t = M.empty
      Inj.instance(x => Prod5[F, A1, A2, A3, A4, A5]((t.t1, t.t2, t.t3, t.t4, x)))
    }

    implicit def Prod5Lens0[F[_], A1, A2, A3, A4, A5]: Lens[Prod5[F, A1, A2, A3, A4, A5], F[A1]] =
      Lens[Prod5[F, A1, A2, A3, A4, A5], F[A1]](p => p.t1)(x => p =>
        Prod5[F, A1, A2, A3, A4, A5]((x, p.t2, p.t3, p.t4, p.t5)))

    implicit def Prod5Lens1[F[_], A1, A2, A3, A4, A5]: Lens[Prod5[F, A1, A2, A3, A4, A5], F[A2]] =
      Lens[Prod5[F, A1, A2, A3, A4, A5], F[A2]](p => p.t2)(x => p =>
        Prod5[F, A1, A2, A3, A4, A5]((p.t1, x, p.t3, p.t4, p.t5)))

    implicit def Prod5Lens2[F[_], A1, A2, A3, A4, A5]: Lens[Prod5[F, A1, A2, A3, A4, A5], F[A3]] =
      Lens[Prod5[F, A1, A2, A3, A4, A5], F[A3]](p => p.t3)(x => p =>
        Prod5[F, A1, A2, A3, A4, A5]((p.t1, p.t2, x, p.t4, p.t5)))

    implicit def Prod5Lens3[F[_], A1, A2, A3, A4, A5]: Lens[Prod5[F, A1, A2, A3, A4, A5], F[A4]] =
      Lens[Prod5[F, A1, A2, A3, A4, A5], F[A4]](p => p.t4)(x => p =>
        Prod5[F, A1, A2, A3, A4, A5]((p.t1, p.t2, p.t3, x, p.t5)))

    implicit def Prod5Lens4[F[_], A1, A2, A3, A4, A5]: Lens[Prod5[F, A1, A2, A3, A4, A5], F[A5]] =
      Lens[Prod5[F, A1, A2, A3, A4, A5], F[A5]](p => p.t5)(x => p =>
        Prod5[F, A1, A2, A3, A4, A5]((p.t1, p.t2, p.t3, p.t4, x)))

  }

  object Prod5 extends Prod5LP {

    implicit def lifta0Id[A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[Id, A1, A2, A3, A4, A5]]): Inj[Prod5[Id, A1, A2, A3, A4, A5], A1] =
      lifta0F[Id, A1, A2, A3, A4, A5]

    implicit def lifta1Id[A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[Id, A1, A2, A3, A4, A5]]): Inj[Prod5[Id, A1, A2, A3, A4, A5], A2] =
      lifta1F[Id, A1, A2, A3, A4, A5]

    implicit def lifta2Id[A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[Id, A1, A2, A3, A4, A5]]): Inj[Prod5[Id, A1, A2, A3, A4, A5], A3] =
      lifta2F[Id, A1, A2, A3, A4, A5]

    implicit def lifta3Id[A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[Id, A1, A2, A3, A4, A5]]): Inj[Prod5[Id, A1, A2, A3, A4, A5], A4] =
      lifta3F[Id, A1, A2, A3, A4, A5]

    implicit def lifta4Id[A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[Id, A1, A2, A3, A4, A5]]): Inj[Prod5[Id, A1, A2, A3, A4, A5], A5] =
      lifta4F[Id, A1, A2, A3, A4, A5]

    implicit def Prod5Lens0Id[A1, A2, A3, A4, A5]: Lens[Prod5[Id, A1, A2, A3, A4, A5], A1] =
      Prod5Lens0[Id, A1, A2, A3, A4, A5]

    implicit def Prod5Lens1Id[A1, A2, A3, A4, A5]: Lens[Prod5[Id, A1, A2, A3, A4, A5], A2] =
      Prod5Lens1[Id, A1, A2, A3, A4, A5]

    implicit def Prod5Lens2Id[A1, A2, A3, A4, A5]: Lens[Prod5[Id, A1, A2, A3, A4, A5], A3] =
      Prod5Lens2[Id, A1, A2, A3, A4, A5]

    implicit def Prod5Lens3Id[A1, A2, A3, A4, A5]: Lens[Prod5[Id, A1, A2, A3, A4, A5], A4] =
      Prod5Lens3[Id, A1, A2, A3, A4, A5]

    implicit def Prod5Lens4Id[A1, A2, A3, A4, A5]: Lens[Prod5[Id, A1, A2, A3, A4, A5], A5] =
      Prod5Lens4[Id, A1, A2, A3, A4, A5]

  }

  @newtype case class Cop5[F[_], A1, A2, A3, A4, A5](run: Either[F[A1], Either[F[A2], Either[F[A3], Either[F[A4], F[A5]]]]]) {
    private def mapN = new Map5C[F[A1], F[A2], F[A3], F[A4], F[A5]] {}

    def map1[B](f: F[A1] => F[B]): Cop5[F, B, A2, A3, A4, A5] =
      Cop5[F, B, A2, A3, A4, A5](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Cop5[F, B, A2, A3, A4, A5] =
      Cop5[F, B, A2, A3, A4, A5](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Cop5[F, A1, B, A3, A4, A5] =
      Cop5[F, A1, B, A3, A4, A5](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Cop5[F, A1, B, A3, A4, A5] =
      Cop5[F, A1, B, A3, A4, A5](mapN.mapAt(f)(run))

    def map3[B](f: F[A3] => F[B]): Cop5[F, A1, A2, B, A4, A5] =
      Cop5[F, A1, A2, B, A4, A5](mapN.map3(run)(f))

    def mapAt[B](f: F[A3] => F[B])(implicit d: Dummy3): Cop5[F, A1, A2, B, A4, A5] =
      Cop5[F, A1, A2, B, A4, A5](mapN.mapAt(f)(run))

    def map4[B](f: F[A4] => F[B]): Cop5[F, A1, A2, A3, B, A5] =
      Cop5[F, A1, A2, A3, B, A5](mapN.map4(run)(f))

    def mapAt[B](f: F[A4] => F[B])(implicit d: Dummy4): Cop5[F, A1, A2, A3, B, A5] =
      Cop5[F, A1, A2, A3, B, A5](mapN.mapAt(f)(run))

    def map5[B](f: F[A5] => F[B]): Cop5[F, A1, A2, A3, A4, B] =
      Cop5[F, A1, A2, A3, A4, B](mapN.map5(run)(f))

    def mapAt[B](f: F[A5] => F[B])(implicit d: Dummy5): Cop5[F, A1, A2, A3, A4, B] =
      Cop5[F, A1, A2, A3, A4, B](mapN.mapAt(f)(run))

  }

  trait Cop5LP {

    implicit def Cop5Instance[A1, A2, A3, A4, A5]: FFunctor[Cop5[*[_], A1, A2, A3, A4, A5]] with FTraverseCop[Cop5[*[_], A1, A2, A3, A4, A5]] =
      new FFunctor[Cop5[*[_], A1, A2, A3, A4, A5]] with FTraverseCop[Cop5[*[_], A1, A2, A3, A4, A5]] {
        def map[F[_], G[_]](c: Cop5[F, A1, A2, A3, A4, A5])(nt: F ~> G): Cop5[G, A1, A2, A3, A4, A5] =
          Cop5[G, A1, A2, A3, A4, A5](c.run.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_))))))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop5[F, A1, A2, A3, A4, A5])(f: F ~> Lambda[a => A[G[a]]]): A[Cop5[G, A1, A2, A3, A4, A5]] =
          c.run match {

            case Left(x) => Functor[A].map(f(x))(y => Cop5[G, A1, A2, A3, A4, A5](Left(y)))

            case Right(Left(x)) => Functor[A].map(f(x))(y => Cop5[G, A1, A2, A3, A4, A5](Right(Left(y))))

            case Right(Right(Left(x))) => Functor[A].map(f(x))(y => Cop5[G, A1, A2, A3, A4, A5](Right(Right(Left(y)))))

            case Right(Right(Right(Left(x)))) => Functor[A].map(f(x))(y => Cop5[G, A1, A2, A3, A4, A5](Right(Right(Right(Left(y))))))

            case Right(Right(Right(Right(x)))) => Functor[A].map(f(x))(y => Cop5[G, A1, A2, A3, A4, A5](Right(Right(Right(Right(y))))))

          }
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5]: Inj[Cop5[F, A1, A2, A3, A4, A5], F[A1]] =
      Inj.instance(x => Cop5[F, A1, A2, A3, A4, A5](Left(x)))

    implicit def inja1F[F[_], A1, A2, A3, A4, A5]: Inj[Cop5[F, A1, A2, A3, A4, A5], F[A2]] =
      Inj.instance(x => Cop5[F, A1, A2, A3, A4, A5](Right(Left(x))))

    implicit def inja2F[F[_], A1, A2, A3, A4, A5]: Inj[Cop5[F, A1, A2, A3, A4, A5], F[A3]] =
      Inj.instance(x => Cop5[F, A1, A2, A3, A4, A5](Right(Right(Left(x)))))

    implicit def inja3F[F[_], A1, A2, A3, A4, A5]: Inj[Cop5[F, A1, A2, A3, A4, A5], F[A4]] =
      Inj.instance(x => Cop5[F, A1, A2, A3, A4, A5](Right(Right(Right(Left(x))))))

    implicit def inja4F[F[_], A1, A2, A3, A4, A5]: Inj[Cop5[F, A1, A2, A3, A4, A5], F[A5]] =
      Inj.instance(x => Cop5[F, A1, A2, A3, A4, A5](Right(Right(Right(Right(x))))))

    implicit def Cop5Optional0[F[_], A1, A2, A3, A4, A5]: Optional[Cop5[F, A1, A2, A3, A4, A5], F[A1]] =
      Optional[Cop5[F, A1, A2, A3, A4, A5], F[A1]](c => c.run match {
        case Left(x) => Some(x)
        case _ => None
      })(x => _ => Cop5[F, A1, A2, A3, A4, A5](Left(x)))

    implicit def Cop5Optional1[F[_], A1, A2, A3, A4, A5]: Optional[Cop5[F, A1, A2, A3, A4, A5], F[A2]] =
      Optional[Cop5[F, A1, A2, A3, A4, A5], F[A2]](c => c.run match {
        case Right(Left(x)) => Some(x)
        case _ => None
      })(x => _ => Cop5[F, A1, A2, A3, A4, A5](Right(Left(x))))

    implicit def Cop5Optional2[F[_], A1, A2, A3, A4, A5]: Optional[Cop5[F, A1, A2, A3, A4, A5], F[A3]] =
      Optional[Cop5[F, A1, A2, A3, A4, A5], F[A3]](c => c.run match {
        case Right(Right(Left(x))) => Some(x)
        case _ => None
      })(x => _ => Cop5[F, A1, A2, A3, A4, A5](Right(Right(Left(x)))))

    implicit def Cop5Optional3[F[_], A1, A2, A3, A4, A5]: Optional[Cop5[F, A1, A2, A3, A4, A5], F[A4]] =
      Optional[Cop5[F, A1, A2, A3, A4, A5], F[A4]](c => c.run match {
        case Right(Right(Right(Left(x)))) => Some(x)
        case _ => None
      })(x => _ => Cop5[F, A1, A2, A3, A4, A5](Right(Right(Right(Left(x))))))

    implicit def Cop5Optional4[F[_], A1, A2, A3, A4, A5]: Optional[Cop5[F, A1, A2, A3, A4, A5], F[A5]] =
      Optional[Cop5[F, A1, A2, A3, A4, A5], F[A5]](c => c.run match {
        case Right(Right(Right(Right(x)))) => Some(x)
        case _ => None
      })(x => _ => Cop5[F, A1, A2, A3, A4, A5](Right(Right(Right(Right(x))))))

  }

  object Cop5 extends Cop5LP {

    implicit def inja0Id[A1, A2, A3, A4, A5]: Inj[Cop5[Id, A1, A2, A3, A4, A5], A1] =
      inja0F[Id, A1, A2, A3, A4, A5]

    implicit def inja1Id[A1, A2, A3, A4, A5]: Inj[Cop5[Id, A1, A2, A3, A4, A5], A2] =
      inja1F[Id, A1, A2, A3, A4, A5]

    implicit def inja2Id[A1, A2, A3, A4, A5]: Inj[Cop5[Id, A1, A2, A3, A4, A5], A3] =
      inja2F[Id, A1, A2, A3, A4, A5]

    implicit def inja3Id[A1, A2, A3, A4, A5]: Inj[Cop5[Id, A1, A2, A3, A4, A5], A4] =
      inja3F[Id, A1, A2, A3, A4, A5]

    implicit def inja4Id[A1, A2, A3, A4, A5]: Inj[Cop5[Id, A1, A2, A3, A4, A5], A5] =
      inja4F[Id, A1, A2, A3, A4, A5]

    implicit def Cop5Optional0Id[A1, A2, A3, A4, A5]: Optional[Cop5[Id, A1, A2, A3, A4, A5], A1] =
      Cop5Optional0[Id, A1, A2, A3, A4, A5]

    implicit def Cop5Optional1Id[A1, A2, A3, A4, A5]: Optional[Cop5[Id, A1, A2, A3, A4, A5], A2] =
      Cop5Optional1[Id, A1, A2, A3, A4, A5]

    implicit def Cop5Optional2Id[A1, A2, A3, A4, A5]: Optional[Cop5[Id, A1, A2, A3, A4, A5], A3] =
      Cop5Optional2[Id, A1, A2, A3, A4, A5]

    implicit def Cop5Optional3Id[A1, A2, A3, A4, A5]: Optional[Cop5[Id, A1, A2, A3, A4, A5], A4] =
      Cop5Optional3[Id, A1, A2, A3, A4, A5]

    implicit def Cop5Optional4Id[A1, A2, A3, A4, A5]: Optional[Cop5[Id, A1, A2, A3, A4, A5], A5] =
      Cop5Optional4[Id, A1, A2, A3, A4, A5]

  }
}
