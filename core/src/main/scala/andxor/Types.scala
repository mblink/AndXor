package andxor

import scalaz.{\/, -\/, \/-, ~>, Applicative, Equal, Functor, Lens, Monoid, PLens, StoreT}
import scalaz.Id.Id
import scalaz.Isomorphism.IsoSet

object types {
  object dummy {

    sealed trait Dummy2; object Dummy2 { implicit val inst: Dummy2 = new Dummy2 {} }
    sealed trait Dummy3; object Dummy3 { implicit val inst: Dummy3 = new Dummy3 {} }
    sealed trait Dummy4; object Dummy4 { implicit val inst: Dummy4 = new Dummy4 {} }
  }

  import dummy._

  @newtype case class ADTValue[A <: Singleton](value: A)
  object ADTValue {
    implicit def equal[A <: Singleton]: Equal[ADTValue[A]] = Equal.equal((a1, a2) => a1.value == a2.value)
  }

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

    implicit def Prod2Instance[A1, A2]: FFunctor[Prod2[?[_], A1, A2]] with FTraverse[Prod2[?[_], A1, A2]] =
      new FFunctor[Prod2[?[_], A1, A2]] with FTraverse[Prod2[?[_], A1, A2]] {
        def map[F[_], G[_]](p: Prod2[F, A1, A2])(nt: F ~> G): Prod2[G, A1, A2] =
          Prod2[G, A1, A2]((nt(p.t1), nt(p.t2)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod2[F, A1, A2])(f: F ~> Lambda[a => A[G[a]]]): A[Prod2[G, A1, A2]] =
          Applicative[A].ap(f(p.t2))(Applicative[A].map(f(p.t1))((i0: G[A1]) => (i1: G[A2]) => Prod2[G, A1, A2]((i0, i1))))
      }

    implicit def Prod2FoldMap[A1, A2]: FoldMap[Prod2[?[_], A1, A2], Cop2[?[_], A1, A2]] =
      new FoldMap[Prod2[?[_], A1, A2], Cop2[?[_], A1, A2]] {
        def unconsAll[F[_], G[_]](p: Prod2[F, A1, A2])(implicit U: Uncons[F, G]): (List[Cop2[G, A1, A2]], Prod2[F, A1, A2]) = {
          val (h1, t1) = U(p.t1)
          val (h2, t2) = U(p.t2)
          (
            List(h1.map(Inj[Cop2[G, A1, A2], G[A1]].apply(_)), h2.map(Inj[Cop2[G, A1, A2], G[A2]].apply(_))).flatten,
            Prod2[F, A1, A2]((t1, t2)))
        }

        def unconsOne[F[_], G[_]](p: Prod2[F, A1, A2], c: Cop2[G, A1, A2])(implicit U: Uncons[F, G]): (Option[Cop2[G, A1, A2]], Prod2[F, A1, A2]) =
          c.run match {

            case -\/(_) =>
              val (h, t) = U(p.t1)
              (h.map(v => Cop2[G, A1, A2](-\/(v))), Prod2[F, A1, A2]((t, p.t2)))

            case \/-(_) =>
              val (h, t) = U(p.t2)
              (h.map(v => Cop2[G, A1, A2](\/-(v))), Prod2[F, A1, A2]((p.t1, t)))

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

  @newtype case class Cop2[F[_], A1, A2](run: (F[A1] \/ F[A2])) {
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

    implicit def Cop2Instance[A1, A2]: FFunctor[Cop2[?[_], A1, A2]] with FTraverse[Cop2[?[_], A1, A2]] =
      new FFunctor[Cop2[?[_], A1, A2]] with FTraverse[Cop2[?[_], A1, A2]] {
        def map[F[_], G[_]](c: Cop2[F, A1, A2])(nt: F ~> G): Cop2[G, A1, A2] =
          Cop2[G, A1, A2](c.run.bimap(nt(_), nt(_)))

        def traverse[F[_], G[_], A[_]: Applicative](c: Cop2[F, A1, A2])(f: F ~> Lambda[a => A[G[a]]]): A[Cop2[G, A1, A2]] =
          c.run match {

            case -\/(x) => Functor[A].map(f(x))(y => Cop2[G, A1, A2](-\/(y)))

            case \/-(x) => Functor[A].map(f(x))(y => Cop2[G, A1, A2](\/-(y)))

          }
      }

    implicit def inja0F[F[_], A1, A2]: Inj[Cop2[F, A1, A2], F[A1]] =
      Inj.instance(x => Cop2[F, A1, A2](-\/(x)))

    implicit def inja1F[F[_], A1, A2]: Inj[Cop2[F, A1, A2], F[A2]] =
      Inj.instance(x => Cop2[F, A1, A2](\/-(x)))

    implicit def Cop2PLens0[F[_], A1, A2]: PLens[Cop2[F, A1, A2], F[A1]] =
      PLens(c => c.run match {
        case -\/(x) => Some(StoreT.store[F[A1], Cop2[F, A1, A2]](x)(y => Cop2[F, A1, A2](-\/(y))))
        case _ => None
      })

    implicit def Cop2PLens1[F[_], A1, A2]: PLens[Cop2[F, A1, A2], F[A2]] =
      PLens(c => c.run match {
        case \/-(x) => Some(StoreT.store[F[A2], Cop2[F, A1, A2]](x)(y => Cop2[F, A1, A2](\/-(y))))
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

    implicit def Prod3Instance[A1, A2, A3]: FFunctor[Prod3[?[_], A1, A2, A3]] with FTraverse[Prod3[?[_], A1, A2, A3]] =
      new FFunctor[Prod3[?[_], A1, A2, A3]] with FTraverse[Prod3[?[_], A1, A2, A3]] {
        def map[F[_], G[_]](p: Prod3[F, A1, A2, A3])(nt: F ~> G): Prod3[G, A1, A2, A3] =
          Prod3[G, A1, A2, A3]((nt(p.t1), nt(p.t2), nt(p.t3)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod3[F, A1, A2, A3])(f: F ~> Lambda[a => A[G[a]]]): A[Prod3[G, A1, A2, A3]] =
          Applicative[A].ap(f(p.t3))(Applicative[A].ap(f(p.t2))(Applicative[A].map(f(p.t1))((i0: G[A1]) => (i1: G[A2]) => (i2: G[A3]) => Prod3[G, A1, A2, A3]((i0, i1, i2)))))
      }

    implicit def Prod3FoldMap[A1, A2, A3]: FoldMap[Prod3[?[_], A1, A2, A3], Cop3[?[_], A1, A2, A3]] =
      new FoldMap[Prod3[?[_], A1, A2, A3], Cop3[?[_], A1, A2, A3]] {
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

            case -\/(_) =>
              val (h, t) = U(p.t1)
              (h.map(v => Cop3[G, A1, A2, A3](-\/(v))), Prod3[F, A1, A2, A3]((t, p.t2, p.t3)))

            case \/-(-\/(_)) =>
              val (h, t) = U(p.t2)
              (h.map(v => Cop3[G, A1, A2, A3](\/-(-\/(v)))), Prod3[F, A1, A2, A3]((p.t1, t, p.t3)))

            case \/-(\/-(_)) =>
              val (h, t) = U(p.t3)
              (h.map(v => Cop3[G, A1, A2, A3](\/-(\/-(v)))), Prod3[F, A1, A2, A3]((p.t1, p.t2, t)))

          }
      }

    def Prod3TupleIso[F[_], A1, A2, A3]: IsoSet[Prod3[F, A1, A2, A3], (F[A1], F[A2], F[A3])] =
      IsoSet((_: Prod3[F, A1, A2, A3]).run, Prod3[F, A1, A2, A3](_: (F[A1], F[A2], F[A3])))

    implicit def Prod3Monoid[F[_], A1, A2, A3](implicit M: Monoid[(F[A1], F[A2], F[A3])]): Monoid[Prod3[F, A1, A2, A3]] =
      Monoid.fromIso(Prod3TupleIso[F, A1, A2, A3])(M)

    implicit def lifta0F[F[_], A1, A2, A3](implicit M: Monoid[Prod3[F, A1, A2, A3]]): Inj[Prod3[F, A1, A2, A3], F[A1]] = {
      val t = M.zero
      Inj.instance(x => Prod3[F, A1, A2, A3]((x, t.t2, t.t3)))
    }

    implicit def lifta1F[F[_], A1, A2, A3](implicit M: Monoid[Prod3[F, A1, A2, A3]]): Inj[Prod3[F, A1, A2, A3], F[A2]] = {
      val t = M.zero
      Inj.instance(x => Prod3[F, A1, A2, A3]((t.t1, x, t.t3)))
    }

    implicit def lifta2F[F[_], A1, A2, A3](implicit M: Monoid[Prod3[F, A1, A2, A3]]): Inj[Prod3[F, A1, A2, A3], F[A3]] = {
      val t = M.zero
      Inj.instance(x => Prod3[F, A1, A2, A3]((t.t1, t.t2, x)))
    }

    implicit def Prod3Lens0[F[_], A1, A2, A3]: Lens[Prod3[F, A1, A2, A3], F[A1]] =
      Lens(p => StoreT.store[F[A1], Prod3[F, A1, A2, A3]](p.t1)(x =>
        Prod3[F, A1, A2, A3]((x, p.t2, p.t3))))

    implicit def Prod3Lens1[F[_], A1, A2, A3]: Lens[Prod3[F, A1, A2, A3], F[A2]] =
      Lens(p => StoreT.store[F[A2], Prod3[F, A1, A2, A3]](p.t2)(x =>
        Prod3[F, A1, A2, A3]((p.t1, x, p.t3))))

    implicit def Prod3Lens2[F[_], A1, A2, A3]: Lens[Prod3[F, A1, A2, A3], F[A3]] =
      Lens(p => StoreT.store[F[A3], Prod3[F, A1, A2, A3]](p.t3)(x =>
        Prod3[F, A1, A2, A3]((p.t1, p.t2, x))))

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

  @newtype case class Cop3[F[_], A1, A2, A3](run: (F[A1] \/ (F[A2] \/ F[A3]))) {
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

    implicit def Cop3Instance[A1, A2, A3]: FFunctor[Cop3[?[_], A1, A2, A3]] with FTraverse[Cop3[?[_], A1, A2, A3]] =
      new FFunctor[Cop3[?[_], A1, A2, A3]] with FTraverse[Cop3[?[_], A1, A2, A3]] {
        def map[F[_], G[_]](c: Cop3[F, A1, A2, A3])(nt: F ~> G): Cop3[G, A1, A2, A3] =
          Cop3[G, A1, A2, A3](c.run.bimap(nt(_), _.bimap(nt(_), nt(_))))

        def traverse[F[_], G[_], A[_]: Applicative](c: Cop3[F, A1, A2, A3])(f: F ~> Lambda[a => A[G[a]]]): A[Cop3[G, A1, A2, A3]] =
          c.run match {

            case -\/(x) => Functor[A].map(f(x))(y => Cop3[G, A1, A2, A3](-\/(y)))

            case \/-(-\/(x)) => Functor[A].map(f(x))(y => Cop3[G, A1, A2, A3](\/-(-\/(y))))

            case \/-(\/-(x)) => Functor[A].map(f(x))(y => Cop3[G, A1, A2, A3](\/-(\/-(y))))

          }
      }

    implicit def inja0F[F[_], A1, A2, A3]: Inj[Cop3[F, A1, A2, A3], F[A1]] =
      Inj.instance(x => Cop3[F, A1, A2, A3](-\/(x)))

    implicit def inja1F[F[_], A1, A2, A3]: Inj[Cop3[F, A1, A2, A3], F[A2]] =
      Inj.instance(x => Cop3[F, A1, A2, A3](\/-(-\/(x))))

    implicit def inja2F[F[_], A1, A2, A3]: Inj[Cop3[F, A1, A2, A3], F[A3]] =
      Inj.instance(x => Cop3[F, A1, A2, A3](\/-(\/-(x))))

    implicit def Cop3PLens0[F[_], A1, A2, A3]: PLens[Cop3[F, A1, A2, A3], F[A1]] =
      PLens(c => c.run match {
        case -\/(x) => Some(StoreT.store[F[A1], Cop3[F, A1, A2, A3]](x)(y => Cop3[F, A1, A2, A3](-\/(y))))
        case _ => None
      })

    implicit def Cop3PLens1[F[_], A1, A2, A3]: PLens[Cop3[F, A1, A2, A3], F[A2]] =
      PLens(c => c.run match {
        case \/-(-\/(x)) => Some(StoreT.store[F[A2], Cop3[F, A1, A2, A3]](x)(y => Cop3[F, A1, A2, A3](\/-(-\/(y)))))
        case _ => None
      })

    implicit def Cop3PLens2[F[_], A1, A2, A3]: PLens[Cop3[F, A1, A2, A3], F[A3]] =
      PLens(c => c.run match {
        case \/-(\/-(x)) => Some(StoreT.store[F[A3], Cop3[F, A1, A2, A3]](x)(y => Cop3[F, A1, A2, A3](\/-(\/-(y)))))
        case _ => None
      })

  }

  object Cop3 extends Cop3LP {

    implicit def inja0Id[A1, A2, A3]: Inj[Cop3[Id, A1, A2, A3], A1] =
      inja0F[Id, A1, A2, A3]

    implicit def inja1Id[A1, A2, A3]: Inj[Cop3[Id, A1, A2, A3], A2] =
      inja1F[Id, A1, A2, A3]

    implicit def inja2Id[A1, A2, A3]: Inj[Cop3[Id, A1, A2, A3], A3] =
      inja2F[Id, A1, A2, A3]

    implicit def Cop3PLens0Id[A1, A2, A3]: PLens[Cop3[Id, A1, A2, A3], A1] =
      Cop3PLens0[Id, A1, A2, A3]

    implicit def Cop3PLens1Id[A1, A2, A3]: PLens[Cop3[Id, A1, A2, A3], A2] =
      Cop3PLens1[Id, A1, A2, A3]

    implicit def Cop3PLens2Id[A1, A2, A3]: PLens[Cop3[Id, A1, A2, A3], A3] =
      Cop3PLens2[Id, A1, A2, A3]

  }

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

    implicit def Prod4Instance[A1, A2, A3, A4]: FFunctor[Prod4[?[_], A1, A2, A3, A4]] with FTraverse[Prod4[?[_], A1, A2, A3, A4]] =
      new FFunctor[Prod4[?[_], A1, A2, A3, A4]] with FTraverse[Prod4[?[_], A1, A2, A3, A4]] {
        def map[F[_], G[_]](p: Prod4[F, A1, A2, A3, A4])(nt: F ~> G): Prod4[G, A1, A2, A3, A4] =
          Prod4[G, A1, A2, A3, A4]((nt(p.t1), nt(p.t2), nt(p.t3), nt(p.t4)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod4[F, A1, A2, A3, A4])(f: F ~> Lambda[a => A[G[a]]]): A[Prod4[G, A1, A2, A3, A4]] =
          Applicative[A].ap(f(p.t4))(Applicative[A].ap(f(p.t3))(Applicative[A].ap(f(p.t2))(Applicative[A].map(f(p.t1))((i0: G[A1]) => (i1: G[A2]) => (i2: G[A3]) => (i3: G[A4]) => Prod4[G, A1, A2, A3, A4]((i0, i1, i2, i3))))))
      }

    implicit def Prod4FoldMap[A1, A2, A3, A4]: FoldMap[Prod4[?[_], A1, A2, A3, A4], Cop4[?[_], A1, A2, A3, A4]] =
      new FoldMap[Prod4[?[_], A1, A2, A3, A4], Cop4[?[_], A1, A2, A3, A4]] {
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

            case -\/(_) =>
              val (h, t) = U(p.t1)
              (h.map(v => Cop4[G, A1, A2, A3, A4](-\/(v))), Prod4[F, A1, A2, A3, A4]((t, p.t2, p.t3, p.t4)))

            case \/-(-\/(_)) =>
              val (h, t) = U(p.t2)
              (h.map(v => Cop4[G, A1, A2, A3, A4](\/-(-\/(v)))), Prod4[F, A1, A2, A3, A4]((p.t1, t, p.t3, p.t4)))

            case \/-(\/-(-\/(_))) =>
              val (h, t) = U(p.t3)
              (h.map(v => Cop4[G, A1, A2, A3, A4](\/-(\/-(-\/(v))))), Prod4[F, A1, A2, A3, A4]((p.t1, p.t2, t, p.t4)))

            case \/-(\/-(\/-(_))) =>
              val (h, t) = U(p.t4)
              (h.map(v => Cop4[G, A1, A2, A3, A4](\/-(\/-(\/-(v))))), Prod4[F, A1, A2, A3, A4]((p.t1, p.t2, p.t3, t)))

          }
      }

    def Prod4TupleIso[F[_], A1, A2, A3, A4]: IsoSet[Prod4[F, A1, A2, A3, A4], (F[A1], F[A2], F[A3], F[A4])] =
      IsoSet((_: Prod4[F, A1, A2, A3, A4]).run, Prod4[F, A1, A2, A3, A4](_: (F[A1], F[A2], F[A3], F[A4])))

    implicit def Prod4Monoid[F[_], A1, A2, A3, A4](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4])]): Monoid[Prod4[F, A1, A2, A3, A4]] =
      Monoid.fromIso(Prod4TupleIso[F, A1, A2, A3, A4])(M)

    implicit def lifta0F[F[_], A1, A2, A3, A4](implicit M: Monoid[Prod4[F, A1, A2, A3, A4]]): Inj[Prod4[F, A1, A2, A3, A4], F[A1]] = {
      val t = M.zero
      Inj.instance(x => Prod4[F, A1, A2, A3, A4]((x, t.t2, t.t3, t.t4)))
    }

    implicit def lifta1F[F[_], A1, A2, A3, A4](implicit M: Monoid[Prod4[F, A1, A2, A3, A4]]): Inj[Prod4[F, A1, A2, A3, A4], F[A2]] = {
      val t = M.zero
      Inj.instance(x => Prod4[F, A1, A2, A3, A4]((t.t1, x, t.t3, t.t4)))
    }

    implicit def lifta2F[F[_], A1, A2, A3, A4](implicit M: Monoid[Prod4[F, A1, A2, A3, A4]]): Inj[Prod4[F, A1, A2, A3, A4], F[A3]] = {
      val t = M.zero
      Inj.instance(x => Prod4[F, A1, A2, A3, A4]((t.t1, t.t2, x, t.t4)))
    }

    implicit def lifta3F[F[_], A1, A2, A3, A4](implicit M: Monoid[Prod4[F, A1, A2, A3, A4]]): Inj[Prod4[F, A1, A2, A3, A4], F[A4]] = {
      val t = M.zero
      Inj.instance(x => Prod4[F, A1, A2, A3, A4]((t.t1, t.t2, t.t3, x)))
    }

    implicit def Prod4Lens0[F[_], A1, A2, A3, A4]: Lens[Prod4[F, A1, A2, A3, A4], F[A1]] =
      Lens(p => StoreT.store[F[A1], Prod4[F, A1, A2, A3, A4]](p.t1)(x =>
        Prod4[F, A1, A2, A3, A4]((x, p.t2, p.t3, p.t4))))

    implicit def Prod4Lens1[F[_], A1, A2, A3, A4]: Lens[Prod4[F, A1, A2, A3, A4], F[A2]] =
      Lens(p => StoreT.store[F[A2], Prod4[F, A1, A2, A3, A4]](p.t2)(x =>
        Prod4[F, A1, A2, A3, A4]((p.t1, x, p.t3, p.t4))))

    implicit def Prod4Lens2[F[_], A1, A2, A3, A4]: Lens[Prod4[F, A1, A2, A3, A4], F[A3]] =
      Lens(p => StoreT.store[F[A3], Prod4[F, A1, A2, A3, A4]](p.t3)(x =>
        Prod4[F, A1, A2, A3, A4]((p.t1, p.t2, x, p.t4))))

    implicit def Prod4Lens3[F[_], A1, A2, A3, A4]: Lens[Prod4[F, A1, A2, A3, A4], F[A4]] =
      Lens(p => StoreT.store[F[A4], Prod4[F, A1, A2, A3, A4]](p.t4)(x =>
        Prod4[F, A1, A2, A3, A4]((p.t1, p.t2, p.t3, x))))

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

  @newtype case class Cop4[F[_], A1, A2, A3, A4](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ F[A4])))) {
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

    implicit def Cop4Instance[A1, A2, A3, A4]: FFunctor[Cop4[?[_], A1, A2, A3, A4]] with FTraverse[Cop4[?[_], A1, A2, A3, A4]] =
      new FFunctor[Cop4[?[_], A1, A2, A3, A4]] with FTraverse[Cop4[?[_], A1, A2, A3, A4]] {
        def map[F[_], G[_]](c: Cop4[F, A1, A2, A3, A4])(nt: F ~> G): Cop4[G, A1, A2, A3, A4] =
          Cop4[G, A1, A2, A3, A4](c.run.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_)))))

        def traverse[F[_], G[_], A[_]: Applicative](c: Cop4[F, A1, A2, A3, A4])(f: F ~> Lambda[a => A[G[a]]]): A[Cop4[G, A1, A2, A3, A4]] =
          c.run match {

            case -\/(x) => Functor[A].map(f(x))(y => Cop4[G, A1, A2, A3, A4](-\/(y)))

            case \/-(-\/(x)) => Functor[A].map(f(x))(y => Cop4[G, A1, A2, A3, A4](\/-(-\/(y))))

            case \/-(\/-(-\/(x))) => Functor[A].map(f(x))(y => Cop4[G, A1, A2, A3, A4](\/-(\/-(-\/(y)))))

            case \/-(\/-(\/-(x))) => Functor[A].map(f(x))(y => Cop4[G, A1, A2, A3, A4](\/-(\/-(\/-(y)))))

          }
      }

    implicit def inja0F[F[_], A1, A2, A3, A4]: Inj[Cop4[F, A1, A2, A3, A4], F[A1]] =
      Inj.instance(x => Cop4[F, A1, A2, A3, A4](-\/(x)))

    implicit def inja1F[F[_], A1, A2, A3, A4]: Inj[Cop4[F, A1, A2, A3, A4], F[A2]] =
      Inj.instance(x => Cop4[F, A1, A2, A3, A4](\/-(-\/(x))))

    implicit def inja2F[F[_], A1, A2, A3, A4]: Inj[Cop4[F, A1, A2, A3, A4], F[A3]] =
      Inj.instance(x => Cop4[F, A1, A2, A3, A4](\/-(\/-(-\/(x)))))

    implicit def inja3F[F[_], A1, A2, A3, A4]: Inj[Cop4[F, A1, A2, A3, A4], F[A4]] =
      Inj.instance(x => Cop4[F, A1, A2, A3, A4](\/-(\/-(\/-(x)))))

    implicit def Cop4PLens0[F[_], A1, A2, A3, A4]: PLens[Cop4[F, A1, A2, A3, A4], F[A1]] =
      PLens(c => c.run match {
        case -\/(x) => Some(StoreT.store[F[A1], Cop4[F, A1, A2, A3, A4]](x)(y => Cop4[F, A1, A2, A3, A4](-\/(y))))
        case _ => None
      })

    implicit def Cop4PLens1[F[_], A1, A2, A3, A4]: PLens[Cop4[F, A1, A2, A3, A4], F[A2]] =
      PLens(c => c.run match {
        case \/-(-\/(x)) => Some(StoreT.store[F[A2], Cop4[F, A1, A2, A3, A4]](x)(y => Cop4[F, A1, A2, A3, A4](\/-(-\/(y)))))
        case _ => None
      })

    implicit def Cop4PLens2[F[_], A1, A2, A3, A4]: PLens[Cop4[F, A1, A2, A3, A4], F[A3]] =
      PLens(c => c.run match {
        case \/-(\/-(-\/(x))) => Some(StoreT.store[F[A3], Cop4[F, A1, A2, A3, A4]](x)(y => Cop4[F, A1, A2, A3, A4](\/-(\/-(-\/(y))))))
        case _ => None
      })

    implicit def Cop4PLens3[F[_], A1, A2, A3, A4]: PLens[Cop4[F, A1, A2, A3, A4], F[A4]] =
      PLens(c => c.run match {
        case \/-(\/-(\/-(x))) => Some(StoreT.store[F[A4], Cop4[F, A1, A2, A3, A4]](x)(y => Cop4[F, A1, A2, A3, A4](\/-(\/-(\/-(y))))))
        case _ => None
      })

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

    implicit def Cop4PLens0Id[A1, A2, A3, A4]: PLens[Cop4[Id, A1, A2, A3, A4], A1] =
      Cop4PLens0[Id, A1, A2, A3, A4]

    implicit def Cop4PLens1Id[A1, A2, A3, A4]: PLens[Cop4[Id, A1, A2, A3, A4], A2] =
      Cop4PLens1[Id, A1, A2, A3, A4]

    implicit def Cop4PLens2Id[A1, A2, A3, A4]: PLens[Cop4[Id, A1, A2, A3, A4], A3] =
      Cop4PLens2[Id, A1, A2, A3, A4]

    implicit def Cop4PLens3Id[A1, A2, A3, A4]: PLens[Cop4[Id, A1, A2, A3, A4], A4] =
      Cop4PLens3[Id, A1, A2, A3, A4]

  }

}
