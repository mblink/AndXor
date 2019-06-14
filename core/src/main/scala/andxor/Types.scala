package andxor

import andxor.Sequence.syntax._
import andxor.Transform.syntax._
import scalaz.{\/, -\/, \/-, ~>, Apply, Functor, InvariantFunctor, Monoid}
import scalaz.Id.Id

object types {
  object dummy {

    sealed trait Dummy1; object Dummy1 { implicit val inst: Dummy1 = new Dummy1 {} }
    sealed trait Dummy2; object Dummy2 { implicit val inst: Dummy2 = new Dummy2 {} }
  }

  private val MF = InvariantFunctor[Monoid]

  @newtype case class Prod1[F[_], A1](run: F[A1]) {
    def t1: F[A1] = run

  }

  trait Prod1LP {
    implicit def Prod1Transform[A1]: Transform[Prod1[?[_], A1]] =
      new Transform[Prod1[?[_], A1]] {
        def transform[F[_], G[_]](nt: F ~> G): Prod1[F, A1] => Prod1[G, A1] =
          p => Prod1[G, A1](nt(p.run))
      }

    implicit def Prod1Sequence[A1]: Sequence[Prod1[?[_], A1], Apply] =
      new Sequence[Prod1[?[_], A1], Apply] {
        def sequence[F[_]](p: Prod1[F, A1])(implicit F: Apply[F]): F[Prod1[Id, A1]] =
          F.map(p.run)((i0: Id[A1]) => Prod1[Id, A1](i0))
      }

    implicit def Prod1FoldMap[A1]: FoldMap[Prod1[?[_], A1], Cop1[?[_], A1]] =
      new FoldMap[Prod1[?[_], A1], Cop1[?[_], A1]] {
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

    implicit def Prod1DerivingProd[TC[_], F[_], A1](implicit tc: TC[F[A1]]): DerivingProd[Prod1[?[_], A1], F, TC] =
      new DerivingProd[Prod1[?[_], A1], F, TC] {
        def mkContravariant[A](f: A => Prod1[F, A1])(implicit D: Divide[TC]): TC[A] =
          D.contramap(tc)(f(_).run)

        def mkCovariant[A](f: Prod1[F, A1] => A)(implicit A: Apply[TC]): TC[A] =
          A.map(tc)(x => f(Prod1[F, A1](x)))
      }

    implicit def Prod1Monoid[F[_], A1](implicit M: Monoid[F[A1]]): Monoid[Prod1[F, A1]] =
      MF.xmap(M, Prod1[F, A1](_: F[A1]), (_: Prod1[F, A1]).run)

    implicit def lifta0F[F[_], A1]: Inj[Prod1[F, A1], F[A1]] =
      {
        Inj.instance(x => Prod1[F, A1](x))
      }

    implicit def lifta0FInverse[F[_], A1]: Inj[F[A1], Prod1[F, A1]] =
      Inj.instance(_.run)
  }

  object Prod1 extends Prod1LP {
    implicit def lifta0Id[A1]: Inj[Prod1[Id, A1], Id[A1]] =

      lifta0F[Id, A1]

    implicit def lifta0IdInverse[A1]: Inj[Id[A1], Prod1[Id, A1]] =
      lifta0FInverse[Id, A1]
  }

  @newtype case class Cop1[F[_], A1](run: F[A1])

  trait Cop1LP {
    implicit def Cop1Transform[A1]: Transform[Cop1[?[_], A1]] =
      new Transform[Cop1[?[_], A1]] {
        def transform[F[_], G[_]](nt: F ~> G): Cop1[F, A1] => Cop1[G, A1] =
          c => Cop1[G, A1](nt(c.run))
      }

    implicit def Cop1Sequence[A1]: Sequence[Cop1[?[_], A1], Functor] =
      new Sequence[Cop1[?[_], A1], Functor] {
        def sequence[F[_]](c: Cop1[F, A1])(implicit F: Functor[F]): F[Cop1[Id, A1]] =
          c.run match {
            case x => F.map(x)(y => Cop1[Id, A1](y))
          }
      }

    implicit def Prod1DerivingCop[TC[_], F[_], A1](implicit tc: TC[F[A1]]): DerivingCop[Cop1[?[_], A1], F, TC] =
      new DerivingCop[Cop1[?[_], A1], F, TC] {
        def mkCovariant[A](f: Cop1[F, A1] => A)(implicit A: Alt[TC]): TC[A] =
          A.map(tc)(x => f(Cop1[F, A1](x)))

        def mkContravariant[A](f: A => Cop1[F, A1])(implicit D: Decidable[TC]): TC[A] =
          D.contramap(tc)(f(_).run)
      }

    implicit def prisma0F[F[_], A1]: Prism[Cop1[F, A1], F[A1]] = new Prism[Cop1[F, A1], F[A1]] {
      def getOption(c: Cop1[F, A1]): Option[F[A1]] = Some(c.run)
      def reverseGet(x: F[A1]): Cop1[F, A1] = Cop1[F, A1](x)
    }

    implicit def inja0F[F[_], A1]: Inj[Cop1[F, A1], F[A1]] = Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1]: Inj[Option[F[A1]], Cop1[F, A1]] = Inj.instance(prisma0F.getOption(_))
  }

  object Cop1 extends Cop1LP {
    implicit def prisma0Id[A1]: Prism[Cop1[Id, A1], Id[A1]] = prisma0F[Id, A1]

    implicit def inja0Id[A1]: Inj[Cop1[Id, A1], Id[A1]] = inja0F[Id, A1]

    implicit def inja0IdInverse[A1]: Inj[Option[Id[A1]], Cop1[Id, A1]] = inja0FInverse[Id, A1]
  }

  @newtype case class Prod2[F[_], A1 <: AndXor, A2 <: AndXor](run: (A1#Prod[F], A2#Prod[F])) {
    def t1: A1#Prod[F] = run._1
    def t2: A2#Prod[F] = run._2

  }

  trait Prod2LP {
    implicit def Prod2Transform[A1 <: AndXor, A2 <: AndXor](implicit trans0: Transform[A1#Prod], trans1: Transform[A2#Prod]): Transform[Prod2[?[_], A1, A2]] =
      new Transform[Prod2[?[_], A1, A2]] {
        def transform[F[_], G[_]](nt: F ~> G): Prod2[F, A1, A2] => Prod2[G, A1, A2] =
          p => Prod2[G, A1, A2]((trans0.transform(nt)(p.t1), trans1.transform(nt)(p.t2)))
      }

    implicit def Prod2Sequence[A1 <: AndXor, A2 <: AndXor](implicit seq0: Sequence[A1#Prod, Apply], seq1: Sequence[A2#Prod, Apply]): Sequence[Prod2[?[_], A1, A2], Apply] =
      new Sequence[Prod2[?[_], A1, A2], Apply] {
        def sequence[F[_]](p: Prod2[F, A1, A2])(implicit F: Apply[F]): F[Prod2[Id, A1, A2]] =
          F.ap(seq1.sequence(p.t2))(F.map(seq0.sequence[F](p.t1))((i0: A1#Prod[Id]) => (i1: A2#Prod[Id]) => Prod2[Id, A1, A2]((i0, i1))))
      }

    implicit def Prod2FoldMap[A1 <: AndXor, A2 <: AndXor](implicit fm0: FoldMap[A1#Prod, A1#Cop], fm1: FoldMap[A2#Prod, A2#Cop]): FoldMap[Prod2[?[_], A1, A2], Cop2[?[_], A1, A2]] =
      new FoldMap[Prod2[?[_], A1, A2], Cop2[?[_], A1, A2]] {
        def unconsAll[F[_], G[_]](p: Prod2[F, A1, A2])(implicit U: Uncons[F, G]): (List[Cop2[G, A1, A2]], Prod2[F, A1, A2]) = {
          val (h1, t1) = fm0.unconsAll(p.t1)
          val (h2, t2) = fm1.unconsAll(p.t2)
          (
            List(h1.map(Inj[Cop2[G, A1, A2], A1#Cop[G]].apply(_)), h2.map(Inj[Cop2[G, A1, A2], A2#Cop[G]].apply(_))).flatten,
            Prod2[F, A1, A2]((t1, t2)))
        }

        def unconsOne[F[_], G[_]](p: Prod2[F, A1, A2], c: Cop2[G, A1, A2])(implicit U: Uncons[F, G]): (Option[Cop2[G, A1, A2]], Prod2[F, A1, A2]) =
          c.run match {
            case -\/(x) =>
              val (h, t) = fm0.unconsOne(p.t1, x)
              (h.map(v => Cop2[G, A1, A2](-\/(v))), Prod2[F, A1, A2]((t, p.t2)))
            case \/-(x) =>
              val (h, t) = fm1.unconsOne(p.t2, x)
              (h.map(v => Cop2[G, A1, A2](\/-(v))), Prod2[F, A1, A2]((p.t1, t)))

          }
      }

    implicit def Prod2DerivingProd[TC[_], F[_], A1 <: AndXor, A2 <: AndXor](implicit deriving0: DerivingProd[A1#Prod, F, TC], deriving1: DerivingProd[A2#Prod, F, TC]): DerivingProd[Prod2[?[_], A1, A2], F, TC] =
      new DerivingProd[Prod2[?[_], A1, A2], F, TC] {
        def mkContravariant[A](f: A => Prod2[F, A1, A2])(implicit D: Divide[TC]): TC[A] =
          Combine.divide2(deriving0.divide, deriving1.divide)(f(_).run)

        def mkCovariant[A](f: Prod2[F, A1, A2] => A)(implicit A: Apply[TC]): TC[A] =

          Combine.apply2(deriving0.apply, deriving1.apply) {
            case (i0, i1) =>
              f(Prod2[F, A1, A2]((i0, i1)))
          }

      }

    implicit def Prod2Monoid[F[_], A1 <: AndXor, A2 <: AndXor](implicit M: Monoid[(A1#Prod[F], A2#Prod[F])]): Monoid[Prod2[F, A1, A2]] =
      MF.xmap(M, Prod2[F, A1, A2](_: (A1#Prod[F], A2#Prod[F])), (_: Prod2[F, A1, A2]).run)

    implicit def lifta0F[F[_], A1 <: AndXor, A2 <: AndXor](implicit M: Monoid[Prod2[F, A1, A2]]): Inj[Prod2[F, A1, A2], A1#Prod[F]] =
      {
        val t = M.zero
        Inj.instance(x => Prod2[F, A1, A2]((x, t.t2)))
      }

    implicit def lifta0FInverse[F[_], A1 <: AndXor, A2 <: AndXor]: Inj[A1#Prod[F], Prod2[F, A1, A2]] =
      Inj.instance(_.t1)

    implicit def liftViaA0F[F[_], A1 <: AndXor, A2 <: AndXor, A](implicit i: Inj[A1#Prod[F], A], M: Monoid[Prod2[F, A1, A2]]): Inj[Prod2[F, A1, A2], A] =
      Inj.instance((i.apply _).andThen(lifta0F[F, A1, A2].apply _))

    implicit def liftViaA0FInverse[F[_], A1 <: AndXor, A2 <: AndXor, A](implicit i: Inj[A, A1#Prod[F]]): Inj[A, Prod2[F, A1, A2]] =
      Inj.instance((lifta0FInverse[F, A1, A2].apply _).andThen(i.apply _))

    implicit def lifta1F[F[_], A1 <: AndXor, A2 <: AndXor](implicit M: Monoid[Prod2[F, A1, A2]]): Inj[Prod2[F, A1, A2], A2#Prod[F]] =
      {
        val t = M.zero
        Inj.instance(x => Prod2[F, A1, A2]((t.t1, x)))
      }

    implicit def lifta1FInverse[F[_], A1 <: AndXor, A2 <: AndXor]: Inj[A2#Prod[F], Prod2[F, A1, A2]] =
      Inj.instance(_.t2)

    implicit def liftViaA1F[F[_], A1 <: AndXor, A2 <: AndXor, A](implicit i: Inj[A2#Prod[F], A], M: Monoid[Prod2[F, A1, A2]]): Inj[Prod2[F, A1, A2], A] =
      Inj.instance((i.apply _).andThen(lifta1F[F, A1, A2].apply _))

    implicit def liftViaA1FInverse[F[_], A1 <: AndXor, A2 <: AndXor, A](implicit i: Inj[A, A2#Prod[F]]): Inj[A, Prod2[F, A1, A2]] =
      Inj.instance((lifta1FInverse[F, A1, A2].apply _).andThen(i.apply _))
  }

  object Prod2 extends Prod2LP {
    implicit def lifta0Id[A1 <: AndXor, A2 <: AndXor](implicit M: Monoid[Prod2[Id, A1, A2]]): Inj[Prod2[Id, A1, A2], A1#Prod[Id]] =

      lifta0F[Id, A1, A2]

    implicit def lifta0IdInverse[A1 <: AndXor, A2 <: AndXor]: Inj[A1#Prod[Id], Prod2[Id, A1, A2]] =
      lifta0FInverse[Id, A1, A2]

    implicit def liftViaA0Id[A1 <: AndXor, A2 <: AndXor, A](implicit i: Inj[A1#Prod[Id], A], M: Monoid[Prod2[Id, A1, A2]]): Inj[Prod2[Id, A1, A2], A] =
      liftViaA0F[Id, A1, A2, A]

    implicit def liftViaA0IdInverse[A1 <: AndXor, A2 <: AndXor, A](implicit i: Inj[A, A1#Prod[Id]]): Inj[A, Prod2[Id, A1, A2]] =
      liftViaA0FInverse[Id, A1, A2, A]

    implicit def lifta1Id[A1 <: AndXor, A2 <: AndXor](implicit M: Monoid[Prod2[Id, A1, A2]]): Inj[Prod2[Id, A1, A2], A2#Prod[Id]] =

      lifta1F[Id, A1, A2]

    implicit def lifta1IdInverse[A1 <: AndXor, A2 <: AndXor]: Inj[A2#Prod[Id], Prod2[Id, A1, A2]] =
      lifta1FInverse[Id, A1, A2]

    implicit def liftViaA1Id[A1 <: AndXor, A2 <: AndXor, A](implicit i: Inj[A2#Prod[Id], A], M: Monoid[Prod2[Id, A1, A2]]): Inj[Prod2[Id, A1, A2], A] =
      liftViaA1F[Id, A1, A2, A]

    implicit def liftViaA1IdInverse[A1 <: AndXor, A2 <: AndXor, A](implicit i: Inj[A, A2#Prod[Id]]): Inj[A, Prod2[Id, A1, A2]] =
      liftViaA1FInverse[Id, A1, A2, A]
  }

  @newtype case class Cop2[F[_], A1 <: AndXor, A2 <: AndXor](run: (A1#Cop[F] \/ A2#Cop[F]))

  trait Cop2LP {
    implicit def Cop2Transform[A1 <: AndXor, A2 <: AndXor](implicit trans0: Transform[A1#Cop], trans1: Transform[A2#Cop]): Transform[Cop2[?[_], A1, A2]] =
      new Transform[Cop2[?[_], A1, A2]] {
        def transform[F[_], G[_]](nt: F ~> G): Cop2[F, A1, A2] => Cop2[G, A1, A2] =
          c => Cop2[G, A1, A2](c.run.bimap(_.transform(nt), _.transform(nt)))
      }

    implicit def Cop2Sequence[A1 <: AndXor, A2 <: AndXor](implicit seq0: Sequence[A1#Cop, Functor], seq1: Sequence[A2#Cop, Functor]): Sequence[Cop2[?[_], A1, A2], Functor] =
      new Sequence[Cop2[?[_], A1, A2], Functor] {
        def sequence[F[_]](c: Cop2[F, A1, A2])(implicit F: Functor[F]): F[Cop2[Id, A1, A2]] =
          c.run match {
            case -\/(x) => F.map(x.sequence)(y => Cop2[Id, A1, A2](-\/(y)))
            case \/-(x) => F.map(x.sequence)(y => Cop2[Id, A1, A2](\/-(y)))
          }
      }

    implicit def Prod2DerivingCop[TC[_], F[_], A1 <: AndXor, A2 <: AndXor](implicit deriving0: DerivingCop[A1#Cop, F, TC], deriving1: DerivingCop[A2#Cop, F, TC]): DerivingCop[Cop2[?[_], A1, A2], F, TC] =
      new DerivingCop[Cop2[?[_], A1, A2], F, TC] {
        def mkCovariant[A](f: Cop2[F, A1, A2] => A)(implicit A: Alt[TC]): TC[A] =
          Combine.altly2(deriving0.alt, deriving1.alt)(x => f(Cop2[F, A1, A2](x)))

        def mkContravariant[A](f: A => Cop2[F, A1, A2])(implicit D: Decidable[TC]): TC[A] =
          Combine.choose2(deriving0.choose, deriving1.choose)(f(_).run)
      }

    implicit def prisma0F[F[_], A1 <: AndXor, A2 <: AndXor]: Prism[Cop2[F, A1, A2], A1#Cop[F]] = new Prism[Cop2[F, A1, A2], A1#Cop[F]] {
      def getOption(c: Cop2[F, A1, A2]): Option[A1#Cop[F]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: A1#Cop[F]): Cop2[F, A1, A2] = Cop2[F, A1, A2](-\/(x))
    }

    implicit def inja0F[F[_], A1 <: AndXor, A2 <: AndXor]: Inj[Cop2[F, A1, A2], A1#Cop[F]] = Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1 <: AndXor, A2 <: AndXor]: Inj[Option[A1#Cop[F]], Cop2[F, A1, A2]] = Inj.instance(prisma0F.getOption(_))

    implicit def injViaA0F[F[_], A1 <: AndXor, A2 <: AndXor, A](implicit i: Inj[A1#Cop[F], A]): Inj[Cop2[F, A1, A2], A] =
      Inj.instance((i.apply _).andThen(inja0F[F, A1, A2].apply _))

    implicit def injViaA0FInverse[F[_], A1 <: AndXor, A2 <: AndXor, A](implicit i: Inj[Option[A], A1#Cop[F]]): Inj[Option[A], Cop2[F, A1, A2]] =
      Inj.instance(inja0FInverse[F, A1, A2](_).flatMap(i(_)))

    implicit def prisma1F[F[_], A1 <: AndXor, A2 <: AndXor]: Prism[Cop2[F, A1, A2], A2#Cop[F]] = new Prism[Cop2[F, A1, A2], A2#Cop[F]] {
      def getOption(c: Cop2[F, A1, A2]): Option[A2#Cop[F]] = c.run match {
        case \/-(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: A2#Cop[F]): Cop2[F, A1, A2] = Cop2[F, A1, A2](\/-(x))
    }

    implicit def inja1F[F[_], A1 <: AndXor, A2 <: AndXor]: Inj[Cop2[F, A1, A2], A2#Cop[F]] = Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1 <: AndXor, A2 <: AndXor]: Inj[Option[A2#Cop[F]], Cop2[F, A1, A2]] = Inj.instance(prisma1F.getOption(_))

    implicit def injViaA1F[F[_], A1 <: AndXor, A2 <: AndXor, A](implicit i: Inj[A2#Cop[F], A]): Inj[Cop2[F, A1, A2], A] =
      Inj.instance((i.apply _).andThen(inja1F[F, A1, A2].apply _))

    implicit def injViaA1FInverse[F[_], A1 <: AndXor, A2 <: AndXor, A](implicit i: Inj[Option[A], A2#Cop[F]]): Inj[Option[A], Cop2[F, A1, A2]] =
      Inj.instance(inja1FInverse[F, A1, A2](_).flatMap(i(_)))
  }

  object Cop2 extends Cop2LP {
    implicit def prisma0Id[A1 <: AndXor, A2 <: AndXor]: Prism[Cop2[Id, A1, A2], A1#Cop[Id]] = prisma0F[Id, A1, A2]

    implicit def inja0Id[A1 <: AndXor, A2 <: AndXor]: Inj[Cop2[Id, A1, A2], A1#Cop[Id]] = inja0F[Id, A1, A2]

    implicit def inja0IdInverse[A1 <: AndXor, A2 <: AndXor]: Inj[Option[A1#Cop[Id]], Cop2[Id, A1, A2]] = inja0FInverse[Id, A1, A2]

    implicit def injViaA0Id[A1 <: AndXor, A2 <: AndXor, A](implicit i: Inj[A1#Cop[Id], A]): Inj[Cop2[Id, A1, A2], A] =
      injViaA0F[Id, A1, A2, A]

    implicit def injViaA0IdInverse[A1 <: AndXor, A2 <: AndXor, A](implicit i: Inj[Option[A], A1#Cop[Id]]): Inj[Option[A], Cop2[Id, A1, A2]] =
      injViaA0FInverse[Id, A1, A2, A]

    implicit def prisma1Id[A1 <: AndXor, A2 <: AndXor]: Prism[Cop2[Id, A1, A2], A2#Cop[Id]] = prisma1F[Id, A1, A2]

    implicit def inja1Id[A1 <: AndXor, A2 <: AndXor]: Inj[Cop2[Id, A1, A2], A2#Cop[Id]] = inja1F[Id, A1, A2]

    implicit def inja1IdInverse[A1 <: AndXor, A2 <: AndXor]: Inj[Option[A2#Cop[Id]], Cop2[Id, A1, A2]] = inja1FInverse[Id, A1, A2]

    implicit def injViaA1Id[A1 <: AndXor, A2 <: AndXor, A](implicit i: Inj[A2#Cop[Id], A]): Inj[Cop2[Id, A1, A2], A] =
      injViaA1F[Id, A1, A2, A]

    implicit def injViaA1IdInverse[A1 <: AndXor, A2 <: AndXor, A](implicit i: Inj[Option[A], A2#Cop[Id]]): Inj[Option[A], Cop2[Id, A1, A2]] =
      injViaA1FInverse[Id, A1, A2, A]
  }

}
