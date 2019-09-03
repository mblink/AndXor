package andxor.types

import andxor._
import scalaz.{\/, -\/, \/-, ~>, Applicative, Functor, Lens, Monoid, PLens, PlusEmpty, StoreT}
import scalaz.Id.Id
import scalaz.Isomorphism.IsoSet

trait Types6 {
  @newtype case class Prod6[F[_], A1, A2, A3, A4, A5, A6](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6])) { self =>
    def t1: F[A1] = run._1
    def t2: F[A2] = run._2
    def t3: F[A3] = run._3
    def t4: F[A4] = run._4
    def t5: F[A5] = run._5
    def t6: F[A6] = run._6

    private def mapN = new Map6P[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6]] {}

    def map1[B](f: F[A1] => F[B]): Prod6[F, B, A2, A3, A4, A5, A6] =
      Prod6[F, B, A2, A3, A4, A5, A6](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Prod6[F, B, A2, A3, A4, A5, A6] =
      Prod6[F, B, A2, A3, A4, A5, A6](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Prod6[F, A1, B, A3, A4, A5, A6] =
      Prod6[F, A1, B, A3, A4, A5, A6](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Prod6[F, A1, B, A3, A4, A5, A6] =
      Prod6[F, A1, B, A3, A4, A5, A6](mapN.mapAt(f)(run))

    def map3[B](f: F[A3] => F[B]): Prod6[F, A1, A2, B, A4, A5, A6] =
      Prod6[F, A1, A2, B, A4, A5, A6](mapN.map3(run)(f))

    def mapAt[B](f: F[A3] => F[B])(implicit d: Dummy3): Prod6[F, A1, A2, B, A4, A5, A6] =
      Prod6[F, A1, A2, B, A4, A5, A6](mapN.mapAt(f)(run))

    def map4[B](f: F[A4] => F[B]): Prod6[F, A1, A2, A3, B, A5, A6] =
      Prod6[F, A1, A2, A3, B, A5, A6](mapN.map4(run)(f))

    def mapAt[B](f: F[A4] => F[B])(implicit d: Dummy4): Prod6[F, A1, A2, A3, B, A5, A6] =
      Prod6[F, A1, A2, A3, B, A5, A6](mapN.mapAt(f)(run))

    def map5[B](f: F[A5] => F[B]): Prod6[F, A1, A2, A3, A4, B, A6] =
      Prod6[F, A1, A2, A3, A4, B, A6](mapN.map5(run)(f))

    def mapAt[B](f: F[A5] => F[B])(implicit d: Dummy5): Prod6[F, A1, A2, A3, A4, B, A6] =
      Prod6[F, A1, A2, A3, A4, B, A6](mapN.mapAt(f)(run))

    def map6[B](f: F[A6] => F[B]): Prod6[F, A1, A2, A3, A4, A5, B] =
      Prod6[F, A1, A2, A3, A4, A5, B](mapN.map6(run)(f))

    def mapAt[B](f: F[A6] => F[B])(implicit d: Dummy6): Prod6[F, A1, A2, A3, A4, A5, B] =
      Prod6[F, A1, A2, A3, A4, A5, B](mapN.mapAt(f)(run))

  }

  trait Prod6LP {

    implicit def Prod6Instance[A1, A2, A3, A4, A5, A6]: FFunctor[Prod6[?[_], A1, A2, A3, A4, A5, A6]] with FTraverseProd[Prod6[?[_], A1, A2, A3, A4, A5, A6]] =
      new FFunctor[Prod6[?[_], A1, A2, A3, A4, A5, A6]] with FTraverseProd[Prod6[?[_], A1, A2, A3, A4, A5, A6]] {
        def map[F[_], G[_]](p: Prod6[F, A1, A2, A3, A4, A5, A6])(nt: F ~> G): Prod6[G, A1, A2, A3, A4, A5, A6] =
          Prod6[G, A1, A2, A3, A4, A5, A6]((nt(p.t1), nt(p.t2), nt(p.t3), nt(p.t4), nt(p.t5), nt(p.t6)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod6[F, A1, A2, A3, A4, A5, A6])(f: F ~> Lambda[a => A[G[a]]]): A[Prod6[G, A1, A2, A3, A4, A5, A6]] =
          Applicative[A].ap(f(p.t6))(Applicative[A].ap(f(p.t5))(Applicative[A].ap(f(p.t4))(Applicative[A].ap(f(p.t3))(Applicative[A].ap(f(p.t2))(Applicative[A].map(f(p.t1))((i0: G[A1]) => (i1: G[A2]) => (i2: G[A3]) => (i3: G[A4]) => (i4: G[A5]) => (i5: G[A6]) => Prod6[G, A1, A2, A3, A4, A5, A6]((i0, i1, i2, i3, i4, i5))))))))
      }

    implicit def Prod6FoldMap[A1, A2, A3, A4, A5, A6]: FoldMap[Prod6[?[_], A1, A2, A3, A4, A5, A6], Cop6[?[_], A1, A2, A3, A4, A5, A6]] =
      new FoldMap[Prod6[?[_], A1, A2, A3, A4, A5, A6], Cop6[?[_], A1, A2, A3, A4, A5, A6]] {
        def emptyProd[F[_]](implicit PE: PlusEmpty[F]): Prod6[F, A1, A2, A3, A4, A5, A6] =
          Prod6[F, A1, A2, A3, A4, A5, A6]((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5], PE.empty[A6]))

        def unconsAll[F[_], G[_]](p: Prod6[F, A1, A2, A3, A4, A5, A6])(implicit U: Uncons[F, G]): (List[Cop6[G, A1, A2, A3, A4, A5, A6]], Prod6[F, A1, A2, A3, A4, A5, A6]) = {
          val (h1, t1) = U(p.t1)
          val (h2, t2) = U(p.t2)
          val (h3, t3) = U(p.t3)
          val (h4, t4) = U(p.t4)
          val (h5, t5) = U(p.t5)
          val (h6, t6) = U(p.t6)
          (
            List(h1.map(Inj[Cop6[G, A1, A2, A3, A4, A5, A6], G[A1]].apply(_)), h2.map(Inj[Cop6[G, A1, A2, A3, A4, A5, A6], G[A2]].apply(_)), h3.map(Inj[Cop6[G, A1, A2, A3, A4, A5, A6], G[A3]].apply(_)), h4.map(Inj[Cop6[G, A1, A2, A3, A4, A5, A6], G[A4]].apply(_)), h5.map(Inj[Cop6[G, A1, A2, A3, A4, A5, A6], G[A5]].apply(_)), h6.map(Inj[Cop6[G, A1, A2, A3, A4, A5, A6], G[A6]].apply(_))).flatten,
            Prod6[F, A1, A2, A3, A4, A5, A6]((t1, t2, t3, t4, t5, t6)))
        }

        def unconsOne[F[_], G[_]](p: Prod6[F, A1, A2, A3, A4, A5, A6], c: Cop6[G, A1, A2, A3, A4, A5, A6])(implicit U: Uncons[F, G]): (Option[Cop6[G, A1, A2, A3, A4, A5, A6]], Prod6[F, A1, A2, A3, A4, A5, A6]) =
          c.run match {

            case -\/(_) =>
              val (h, t) = U(p.t1)
              (h.map(v => Cop6[G, A1, A2, A3, A4, A5, A6](-\/(v))), Prod6[F, A1, A2, A3, A4, A5, A6]((t, p.t2, p.t3, p.t4, p.t5, p.t6)))

            case \/-(-\/(_)) =>
              val (h, t) = U(p.t2)
              (h.map(v => Cop6[G, A1, A2, A3, A4, A5, A6](\/-(-\/(v)))), Prod6[F, A1, A2, A3, A4, A5, A6]((p.t1, t, p.t3, p.t4, p.t5, p.t6)))

            case \/-(\/-(-\/(_))) =>
              val (h, t) = U(p.t3)
              (h.map(v => Cop6[G, A1, A2, A3, A4, A5, A6](\/-(\/-(-\/(v))))), Prod6[F, A1, A2, A3, A4, A5, A6]((p.t1, p.t2, t, p.t4, p.t5, p.t6)))

            case \/-(\/-(\/-(-\/(_)))) =>
              val (h, t) = U(p.t4)
              (h.map(v => Cop6[G, A1, A2, A3, A4, A5, A6](\/-(\/-(\/-(-\/(v)))))), Prod6[F, A1, A2, A3, A4, A5, A6]((p.t1, p.t2, p.t3, t, p.t5, p.t6)))

            case \/-(\/-(\/-(\/-(-\/(_))))) =>
              val (h, t) = U(p.t5)
              (h.map(v => Cop6[G, A1, A2, A3, A4, A5, A6](\/-(\/-(\/-(\/-(-\/(v))))))), Prod6[F, A1, A2, A3, A4, A5, A6]((p.t1, p.t2, p.t3, p.t4, t, p.t6)))

            case \/-(\/-(\/-(\/-(\/-(_))))) =>
              val (h, t) = U(p.t6)
              (h.map(v => Cop6[G, A1, A2, A3, A4, A5, A6](\/-(\/-(\/-(\/-(\/-(v))))))), Prod6[F, A1, A2, A3, A4, A5, A6]((p.t1, p.t2, p.t3, p.t4, p.t5, t)))

          }
      }

    def Prod6TupleIso[F[_], A1, A2, A3, A4, A5, A6]: IsoSet[Prod6[F, A1, A2, A3, A4, A5, A6], (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6])] =
      IsoSet((_: Prod6[F, A1, A2, A3, A4, A5, A6]).run, Prod6[F, A1, A2, A3, A4, A5, A6](_: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6])))

    implicit def Prod6Monoid[F[_], A1, A2, A3, A4, A5, A6](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6])]): Monoid[Prod6[F, A1, A2, A3, A4, A5, A6]] =
      Monoid.fromIso(Prod6TupleIso[F, A1, A2, A3, A4, A5, A6])(M)

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[F, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[F, A1, A2, A3, A4, A5, A6], F[A1]] = {
      val t = M.zero
      Inj.instance(x => Prod6[F, A1, A2, A3, A4, A5, A6]((x, t.t2, t.t3, t.t4, t.t5, t.t6)))
    }

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[F, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[F, A1, A2, A3, A4, A5, A6], F[A2]] = {
      val t = M.zero
      Inj.instance(x => Prod6[F, A1, A2, A3, A4, A5, A6]((t.t1, x, t.t3, t.t4, t.t5, t.t6)))
    }

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[F, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[F, A1, A2, A3, A4, A5, A6], F[A3]] = {
      val t = M.zero
      Inj.instance(x => Prod6[F, A1, A2, A3, A4, A5, A6]((t.t1, t.t2, x, t.t4, t.t5, t.t6)))
    }

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[F, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[F, A1, A2, A3, A4, A5, A6], F[A4]] = {
      val t = M.zero
      Inj.instance(x => Prod6[F, A1, A2, A3, A4, A5, A6]((t.t1, t.t2, t.t3, x, t.t5, t.t6)))
    }

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[F, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[F, A1, A2, A3, A4, A5, A6], F[A5]] = {
      val t = M.zero
      Inj.instance(x => Prod6[F, A1, A2, A3, A4, A5, A6]((t.t1, t.t2, t.t3, t.t4, x, t.t6)))
    }

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[F, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[F, A1, A2, A3, A4, A5, A6], F[A6]] = {
      val t = M.zero
      Inj.instance(x => Prod6[F, A1, A2, A3, A4, A5, A6]((t.t1, t.t2, t.t3, t.t4, t.t5, x)))
    }

    implicit def Prod6Lens0[F[_], A1, A2, A3, A4, A5, A6]: Lens[Prod6[F, A1, A2, A3, A4, A5, A6], F[A1]] =
      Lens(p => StoreT.store[F[A1], Prod6[F, A1, A2, A3, A4, A5, A6]](p.t1)(x =>
        Prod6[F, A1, A2, A3, A4, A5, A6]((x, p.t2, p.t3, p.t4, p.t5, p.t6))))

    implicit def Prod6Lens1[F[_], A1, A2, A3, A4, A5, A6]: Lens[Prod6[F, A1, A2, A3, A4, A5, A6], F[A2]] =
      Lens(p => StoreT.store[F[A2], Prod6[F, A1, A2, A3, A4, A5, A6]](p.t2)(x =>
        Prod6[F, A1, A2, A3, A4, A5, A6]((p.t1, x, p.t3, p.t4, p.t5, p.t6))))

    implicit def Prod6Lens2[F[_], A1, A2, A3, A4, A5, A6]: Lens[Prod6[F, A1, A2, A3, A4, A5, A6], F[A3]] =
      Lens(p => StoreT.store[F[A3], Prod6[F, A1, A2, A3, A4, A5, A6]](p.t3)(x =>
        Prod6[F, A1, A2, A3, A4, A5, A6]((p.t1, p.t2, x, p.t4, p.t5, p.t6))))

    implicit def Prod6Lens3[F[_], A1, A2, A3, A4, A5, A6]: Lens[Prod6[F, A1, A2, A3, A4, A5, A6], F[A4]] =
      Lens(p => StoreT.store[F[A4], Prod6[F, A1, A2, A3, A4, A5, A6]](p.t4)(x =>
        Prod6[F, A1, A2, A3, A4, A5, A6]((p.t1, p.t2, p.t3, x, p.t5, p.t6))))

    implicit def Prod6Lens4[F[_], A1, A2, A3, A4, A5, A6]: Lens[Prod6[F, A1, A2, A3, A4, A5, A6], F[A5]] =
      Lens(p => StoreT.store[F[A5], Prod6[F, A1, A2, A3, A4, A5, A6]](p.t5)(x =>
        Prod6[F, A1, A2, A3, A4, A5, A6]((p.t1, p.t2, p.t3, p.t4, x, p.t6))))

    implicit def Prod6Lens5[F[_], A1, A2, A3, A4, A5, A6]: Lens[Prod6[F, A1, A2, A3, A4, A5, A6], F[A6]] =
      Lens(p => StoreT.store[F[A6], Prod6[F, A1, A2, A3, A4, A5, A6]](p.t6)(x =>
        Prod6[F, A1, A2, A3, A4, A5, A6]((p.t1, p.t2, p.t3, p.t4, p.t5, x))))

  }

  object Prod6 extends Prod6LP {

    implicit def lifta0Id[A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[Id, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[Id, A1, A2, A3, A4, A5, A6], A1] =
      lifta0F[Id, A1, A2, A3, A4, A5, A6]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[Id, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[Id, A1, A2, A3, A4, A5, A6], A2] =
      lifta1F[Id, A1, A2, A3, A4, A5, A6]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[Id, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[Id, A1, A2, A3, A4, A5, A6], A3] =
      lifta2F[Id, A1, A2, A3, A4, A5, A6]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[Id, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[Id, A1, A2, A3, A4, A5, A6], A4] =
      lifta3F[Id, A1, A2, A3, A4, A5, A6]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[Id, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[Id, A1, A2, A3, A4, A5, A6], A5] =
      lifta4F[Id, A1, A2, A3, A4, A5, A6]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[Id, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[Id, A1, A2, A3, A4, A5, A6], A6] =
      lifta5F[Id, A1, A2, A3, A4, A5, A6]

    implicit def Prod6Lens0Id[A1, A2, A3, A4, A5, A6]: Lens[Prod6[Id, A1, A2, A3, A4, A5, A6], A1] =
      Prod6Lens0[Id, A1, A2, A3, A4, A5, A6]

    implicit def Prod6Lens1Id[A1, A2, A3, A4, A5, A6]: Lens[Prod6[Id, A1, A2, A3, A4, A5, A6], A2] =
      Prod6Lens1[Id, A1, A2, A3, A4, A5, A6]

    implicit def Prod6Lens2Id[A1, A2, A3, A4, A5, A6]: Lens[Prod6[Id, A1, A2, A3, A4, A5, A6], A3] =
      Prod6Lens2[Id, A1, A2, A3, A4, A5, A6]

    implicit def Prod6Lens3Id[A1, A2, A3, A4, A5, A6]: Lens[Prod6[Id, A1, A2, A3, A4, A5, A6], A4] =
      Prod6Lens3[Id, A1, A2, A3, A4, A5, A6]

    implicit def Prod6Lens4Id[A1, A2, A3, A4, A5, A6]: Lens[Prod6[Id, A1, A2, A3, A4, A5, A6], A5] =
      Prod6Lens4[Id, A1, A2, A3, A4, A5, A6]

    implicit def Prod6Lens5Id[A1, A2, A3, A4, A5, A6]: Lens[Prod6[Id, A1, A2, A3, A4, A5, A6], A6] =
      Prod6Lens5[Id, A1, A2, A3, A4, A5, A6]

  }

  @newtype case class Cop6[F[_], A1, A2, A3, A4, A5, A6](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ F[A6])))))) {
    private def mapN = new Map6C[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6]] {}

    def map1[B](f: F[A1] => F[B]): Cop6[F, B, A2, A3, A4, A5, A6] =
      Cop6[F, B, A2, A3, A4, A5, A6](mapN.map1(run)(f))

    def mapAt[B](f: F[A1] => F[B]): Cop6[F, B, A2, A3, A4, A5, A6] =
      Cop6[F, B, A2, A3, A4, A5, A6](mapN.mapAt(f)(run))

    def map2[B](f: F[A2] => F[B]): Cop6[F, A1, B, A3, A4, A5, A6] =
      Cop6[F, A1, B, A3, A4, A5, A6](mapN.map2(run)(f))

    def mapAt[B](f: F[A2] => F[B])(implicit d: Dummy2): Cop6[F, A1, B, A3, A4, A5, A6] =
      Cop6[F, A1, B, A3, A4, A5, A6](mapN.mapAt(f)(run))

    def map3[B](f: F[A3] => F[B]): Cop6[F, A1, A2, B, A4, A5, A6] =
      Cop6[F, A1, A2, B, A4, A5, A6](mapN.map3(run)(f))

    def mapAt[B](f: F[A3] => F[B])(implicit d: Dummy3): Cop6[F, A1, A2, B, A4, A5, A6] =
      Cop6[F, A1, A2, B, A4, A5, A6](mapN.mapAt(f)(run))

    def map4[B](f: F[A4] => F[B]): Cop6[F, A1, A2, A3, B, A5, A6] =
      Cop6[F, A1, A2, A3, B, A5, A6](mapN.map4(run)(f))

    def mapAt[B](f: F[A4] => F[B])(implicit d: Dummy4): Cop6[F, A1, A2, A3, B, A5, A6] =
      Cop6[F, A1, A2, A3, B, A5, A6](mapN.mapAt(f)(run))

    def map5[B](f: F[A5] => F[B]): Cop6[F, A1, A2, A3, A4, B, A6] =
      Cop6[F, A1, A2, A3, A4, B, A6](mapN.map5(run)(f))

    def mapAt[B](f: F[A5] => F[B])(implicit d: Dummy5): Cop6[F, A1, A2, A3, A4, B, A6] =
      Cop6[F, A1, A2, A3, A4, B, A6](mapN.mapAt(f)(run))

    def map6[B](f: F[A6] => F[B]): Cop6[F, A1, A2, A3, A4, A5, B] =
      Cop6[F, A1, A2, A3, A4, A5, B](mapN.map6(run)(f))

    def mapAt[B](f: F[A6] => F[B])(implicit d: Dummy6): Cop6[F, A1, A2, A3, A4, A5, B] =
      Cop6[F, A1, A2, A3, A4, A5, B](mapN.mapAt(f)(run))

  }

  trait Cop6LP {

    implicit def Cop6Instance[A1, A2, A3, A4, A5, A6]: FFunctor[Cop6[?[_], A1, A2, A3, A4, A5, A6]] with FTraverseCop[Cop6[?[_], A1, A2, A3, A4, A5, A6]] =
      new FFunctor[Cop6[?[_], A1, A2, A3, A4, A5, A6]] with FTraverseCop[Cop6[?[_], A1, A2, A3, A4, A5, A6]] {
        def map[F[_], G[_]](c: Cop6[F, A1, A2, A3, A4, A5, A6])(nt: F ~> G): Cop6[G, A1, A2, A3, A4, A5, A6] =
          Cop6[G, A1, A2, A3, A4, A5, A6](c.run.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_)))))))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop6[F, A1, A2, A3, A4, A5, A6])(f: F ~> Lambda[a => A[G[a]]]): A[Cop6[G, A1, A2, A3, A4, A5, A6]] =
          c.run match {

            case -\/(x) => Functor[A].map(f(x))(y => Cop6[G, A1, A2, A3, A4, A5, A6](-\/(y)))

            case \/-(-\/(x)) => Functor[A].map(f(x))(y => Cop6[G, A1, A2, A3, A4, A5, A6](\/-(-\/(y))))

            case \/-(\/-(-\/(x))) => Functor[A].map(f(x))(y => Cop6[G, A1, A2, A3, A4, A5, A6](\/-(\/-(-\/(y)))))

            case \/-(\/-(\/-(-\/(x)))) => Functor[A].map(f(x))(y => Cop6[G, A1, A2, A3, A4, A5, A6](\/-(\/-(\/-(-\/(y))))))

            case \/-(\/-(\/-(\/-(-\/(x))))) => Functor[A].map(f(x))(y => Cop6[G, A1, A2, A3, A4, A5, A6](\/-(\/-(\/-(\/-(-\/(y)))))))

            case \/-(\/-(\/-(\/-(\/-(x))))) => Functor[A].map(f(x))(y => Cop6[G, A1, A2, A3, A4, A5, A6](\/-(\/-(\/-(\/-(\/-(y)))))))

          }
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6]: Inj[Cop6[F, A1, A2, A3, A4, A5, A6], F[A1]] =
      Inj.instance(x => Cop6[F, A1, A2, A3, A4, A5, A6](-\/(x)))

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6]: Inj[Cop6[F, A1, A2, A3, A4, A5, A6], F[A2]] =
      Inj.instance(x => Cop6[F, A1, A2, A3, A4, A5, A6](\/-(-\/(x))))

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6]: Inj[Cop6[F, A1, A2, A3, A4, A5, A6], F[A3]] =
      Inj.instance(x => Cop6[F, A1, A2, A3, A4, A5, A6](\/-(\/-(-\/(x)))))

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6]: Inj[Cop6[F, A1, A2, A3, A4, A5, A6], F[A4]] =
      Inj.instance(x => Cop6[F, A1, A2, A3, A4, A5, A6](\/-(\/-(\/-(-\/(x))))))

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6]: Inj[Cop6[F, A1, A2, A3, A4, A5, A6], F[A5]] =
      Inj.instance(x => Cop6[F, A1, A2, A3, A4, A5, A6](\/-(\/-(\/-(\/-(-\/(x)))))))

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6]: Inj[Cop6[F, A1, A2, A3, A4, A5, A6], F[A6]] =
      Inj.instance(x => Cop6[F, A1, A2, A3, A4, A5, A6](\/-(\/-(\/-(\/-(\/-(x)))))))

    implicit def Cop6PLens0[F[_], A1, A2, A3, A4, A5, A6]: PLens[Cop6[F, A1, A2, A3, A4, A5, A6], F[A1]] =
      PLens(c => c.run match {
        case -\/(x) => Some(StoreT.store[F[A1], Cop6[F, A1, A2, A3, A4, A5, A6]](x)(y => Cop6[F, A1, A2, A3, A4, A5, A6](-\/(y))))
        case _ => None
      })

    implicit def Cop6PLens1[F[_], A1, A2, A3, A4, A5, A6]: PLens[Cop6[F, A1, A2, A3, A4, A5, A6], F[A2]] =
      PLens(c => c.run match {
        case \/-(-\/(x)) => Some(StoreT.store[F[A2], Cop6[F, A1, A2, A3, A4, A5, A6]](x)(y => Cop6[F, A1, A2, A3, A4, A5, A6](\/-(-\/(y)))))
        case _ => None
      })

    implicit def Cop6PLens2[F[_], A1, A2, A3, A4, A5, A6]: PLens[Cop6[F, A1, A2, A3, A4, A5, A6], F[A3]] =
      PLens(c => c.run match {
        case \/-(\/-(-\/(x))) => Some(StoreT.store[F[A3], Cop6[F, A1, A2, A3, A4, A5, A6]](x)(y => Cop6[F, A1, A2, A3, A4, A5, A6](\/-(\/-(-\/(y))))))
        case _ => None
      })

    implicit def Cop6PLens3[F[_], A1, A2, A3, A4, A5, A6]: PLens[Cop6[F, A1, A2, A3, A4, A5, A6], F[A4]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(StoreT.store[F[A4], Cop6[F, A1, A2, A3, A4, A5, A6]](x)(y => Cop6[F, A1, A2, A3, A4, A5, A6](\/-(\/-(\/-(-\/(y)))))))
        case _ => None
      })

    implicit def Cop6PLens4[F[_], A1, A2, A3, A4, A5, A6]: PLens[Cop6[F, A1, A2, A3, A4, A5, A6], F[A5]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(StoreT.store[F[A5], Cop6[F, A1, A2, A3, A4, A5, A6]](x)(y => Cop6[F, A1, A2, A3, A4, A5, A6](\/-(\/-(\/-(\/-(-\/(y))))))))
        case _ => None
      })

    implicit def Cop6PLens5[F[_], A1, A2, A3, A4, A5, A6]: PLens[Cop6[F, A1, A2, A3, A4, A5, A6], F[A6]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(\/-(\/-(x))))) => Some(StoreT.store[F[A6], Cop6[F, A1, A2, A3, A4, A5, A6]](x)(y => Cop6[F, A1, A2, A3, A4, A5, A6](\/-(\/-(\/-(\/-(\/-(y))))))))
        case _ => None
      })

  }

  object Cop6 extends Cop6LP {

    implicit def inja0Id[A1, A2, A3, A4, A5, A6]: Inj[Cop6[Id, A1, A2, A3, A4, A5, A6], A1] =
      inja0F[Id, A1, A2, A3, A4, A5, A6]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6]: Inj[Cop6[Id, A1, A2, A3, A4, A5, A6], A2] =
      inja1F[Id, A1, A2, A3, A4, A5, A6]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6]: Inj[Cop6[Id, A1, A2, A3, A4, A5, A6], A3] =
      inja2F[Id, A1, A2, A3, A4, A5, A6]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6]: Inj[Cop6[Id, A1, A2, A3, A4, A5, A6], A4] =
      inja3F[Id, A1, A2, A3, A4, A5, A6]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6]: Inj[Cop6[Id, A1, A2, A3, A4, A5, A6], A5] =
      inja4F[Id, A1, A2, A3, A4, A5, A6]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6]: Inj[Cop6[Id, A1, A2, A3, A4, A5, A6], A6] =
      inja5F[Id, A1, A2, A3, A4, A5, A6]

    implicit def Cop6PLens0Id[A1, A2, A3, A4, A5, A6]: PLens[Cop6[Id, A1, A2, A3, A4, A5, A6], A1] =
      Cop6PLens0[Id, A1, A2, A3, A4, A5, A6]

    implicit def Cop6PLens1Id[A1, A2, A3, A4, A5, A6]: PLens[Cop6[Id, A1, A2, A3, A4, A5, A6], A2] =
      Cop6PLens1[Id, A1, A2, A3, A4, A5, A6]

    implicit def Cop6PLens2Id[A1, A2, A3, A4, A5, A6]: PLens[Cop6[Id, A1, A2, A3, A4, A5, A6], A3] =
      Cop6PLens2[Id, A1, A2, A3, A4, A5, A6]

    implicit def Cop6PLens3Id[A1, A2, A3, A4, A5, A6]: PLens[Cop6[Id, A1, A2, A3, A4, A5, A6], A4] =
      Cop6PLens3[Id, A1, A2, A3, A4, A5, A6]

    implicit def Cop6PLens4Id[A1, A2, A3, A4, A5, A6]: PLens[Cop6[Id, A1, A2, A3, A4, A5, A6], A5] =
      Cop6PLens4[Id, A1, A2, A3, A4, A5, A6]

    implicit def Cop6PLens5Id[A1, A2, A3, A4, A5, A6]: PLens[Cop6[Id, A1, A2, A3, A4, A5, A6], A6] =
      Cop6PLens5[Id, A1, A2, A3, A4, A5, A6]

  }
}
