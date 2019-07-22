package andxor

import andxor.syntax.ffunctor._
import andxor.syntax.ftraverse._
import andxor.types._
import scalaz.{~>, \/, -\/, \/-, Applicative, Functor, Apply, Monoid}
import scalaz.Id.Id
import scalaz.std.vector._

trait AndXorNested3[A1[_[_]], A2[_[_]], A3[_[_]]] extends AndXor {

  def apply[B1]: AndXorNested4[A1, A2, A3, FConst[B1]#T] = AndXorNested4[A1, A2, A3, FConst[B1]#T]
  def nest[B1[_[_]]]: AndXorNested4[A1, A2, A3, B1] = AndXorNested4[A1, A2, A3, B1]

  type Prod[F[_]] = Prod3[Id, A1[F], A2[F], A3[F]]
  object Prod {
    def apply[F[_]](p: (A1[F], A2[F], A3[F])): Prod[F] = Prod3[Id, A1[F], A2[F], A3[F]](p)
  }

  type Cop[F[_]] = Cop3[Id, A1[F], A2[F], A3[F]]
  object Cop {
    def apply[F[_]](c: (A1[F] \/ (A2[F] \/ A3[F]))): Cop[F] = Cop3[Id, A1[F], A2[F], A3[F]](c)
  }

  object instances {
    implicit def axoProd3Instance(implicit ft0: FTraverse[A1], ft1: FTraverse[A2], ft2: FTraverse[A3]): FFunctor[Prod] with FTraverse[Prod] =
      new FFunctor[Prod] with FTraverse[Prod] {
        def map[F[_], G[_]](p: Prod3[Id, A1[F], A2[F], A3[F]])(nt: F ~> G): Prod3[Id, A1[G], A2[G], A3[G]] =
          Prod3[Id, A1[G], A2[G], A3[G]]((ft0.map(p.t1)(nt), ft1.map(p.t2)(nt), ft2.map(p.t3)(nt)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod3[Id, A1[F], A2[F], A3[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Prod3[Id, A1[G], A2[G], A3[G]]] =
          Applicative[A].ap(ft2.traverse(p.t3)(f))(Applicative[A].ap(ft1.traverse(p.t2)(f))(Applicative[A].map(ft0.traverse(p.t1)(f))((i0: A1[G]) => (i1: A2[G]) => (i2: A3[G]) => Prod3[Id, A1[G], A2[G], A3[G]]((i0, i1, i2)))))
      }

    implicit def axoProd3FoldMap(implicit fm0: FoldMap[A1, A1], fm1: FoldMap[A2, A2], fm2: FoldMap[A3, A3]): FoldMap[Prod, Cop] =
      new FoldMap[Prod, Cop] {
        def unconsAll[F[_], G[_]](p: Prod3[Id, A1[F], A2[F], A3[F]])(implicit U: Uncons[F, G]): (List[Cop3[Id, A1[G], A2[G], A3[G]]], Prod3[Id, A1[F], A2[F], A3[F]]) = {
          val (h1, t1) = fm0.unconsAll(p.t1)
          val (h2, t2) = fm1.unconsAll(p.t2)
          val (h3, t3) = fm2.unconsAll(p.t3)
          (
            List(h1.map(Inj[Cop3[Id, A1[G], A2[G], A3[G]], A1[G]].apply(_)), h2.map(Inj[Cop3[Id, A1[G], A2[G], A3[G]], A2[G]].apply(_)), h3.map(Inj[Cop3[Id, A1[G], A2[G], A3[G]], A3[G]].apply(_))).flatten,
            Prod3[Id, A1[F], A2[F], A3[F]]((t1, t2, t3)))
        }

        def unconsOne[F[_], G[_]](p: Prod3[Id, A1[F], A2[F], A3[F]], c: Cop3[Id, A1[G], A2[G], A3[G]])(implicit U: Uncons[F, G]): (Option[Cop3[Id, A1[G], A2[G], A3[G]]], Prod3[Id, A1[F], A2[F], A3[F]]) =
          c.run match {

            case -\/(x) =>
              val (h, t) = fm0.unconsOne(p.t1, x)
              (h.map(v => Cop3[Id, A1[G], A2[G], A3[G]](-\/(v))), Prod3[Id, A1[F], A2[F], A3[F]]((t, p.t2, p.t3)))

            case \/-(-\/(x)) =>
              val (h, t) = fm1.unconsOne(p.t2, x)
              (h.map(v => Cop3[Id, A1[G], A2[G], A3[G]](\/-(-\/(v)))), Prod3[Id, A1[F], A2[F], A3[F]]((p.t1, t, p.t3)))

            case \/-(\/-(x)) =>
              val (h, t) = fm2.unconsOne(p.t3, x)
              (h.map(v => Cop3[Id, A1[G], A2[G], A3[G]](\/-(\/-(v)))), Prod3[Id, A1[F], A2[F], A3[F]]((p.t1, p.t2, t)))

          }
      }

    implicit def axoCop3Instance(implicit ft0: FTraverse[A1], ft1: FTraverse[A2], ft2: FTraverse[A3]): FFunctor[Cop] with FTraverse[Cop] =
      new FFunctor[Cop] with FTraverse[Cop] {
        def map[F[_], G[_]](c: Cop3[Id, A1[F], A2[F], A3[F]])(nt: F ~> G): Cop3[Id, A1[G], A2[G], A3[G]] =
          Cop3[Id, A1[G], A2[G], A3[G]](c.run.bimap(_.map(nt), _.bimap(_.map(nt), _.map(nt))))

        def traverse[F[_], G[_], A[_]: Applicative](c: Cop3[Id, A1[F], A2[F], A3[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Cop3[Id, A1[G], A2[G], A3[G]]] =
          c.run match {

            case -\/(x) => Functor[A].map(x.traverse(f))(y => Cop3[Id, A1[G], A2[G], A3[G]](-\/(y)))

            case \/-(-\/(x)) => Functor[A].map(x.traverse(f))(y => Cop3[Id, A1[G], A2[G], A3[G]](\/-(-\/(y))))

            case \/-(\/-(x)) => Functor[A].map(x.traverse(f))(y => Cop3[Id, A1[G], A2[G], A3[G]](\/-(\/-(y))))

          }
      }
  }

  def deriving[TC[_], F[_]](implicit t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose3(t0, t1, t2)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly3(t0, t1, t2)(x => f(Cop3[Id, A1[F], A2[F], A3[F]](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide3(t0, t1, t2)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply3(t0, t1, t2) {
          case (i0, i1, i2) =>
            f(Prod3[Id, A1[F], A2[F], A3[F]]((i0, i1, i2)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1[Id]], t1: TC[A2[Id]], t2: TC[A3[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = deriving[Inj[Prod[F], ?], F].divide
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]] = deriving[Inj[Prod[F], ?], F].choose
    implicit def injProdToVecCopEv[F[_]]: Inj[Vector[Cop[F]], Prod[F]] = deriving[Inj[Vector[Cop[F]], ?], F].divide
  }

}

object AndXorNested3 {
  def apply[A1[_[_]], A2[_[_]], A3[_[_]]]: AndXorNested3[A1, A2, A3] =
    new AndXorNested3[A1, A2, A3] {}
}

trait AndXor3[A1, A2, A3] extends AndXor {

  def apply[B1]: AndXor4[A1, A2, A3, B1] = AndXor4[A1, A2, A3, B1]
  def nest[B1[_[_]]]: AndXorNested4[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, B1] = AndXorNested4[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, B1]

  type Prod[F[_]] = Prod3[F, A1, A2, A3]
  object Prod {
    def apply[F[_]](p: (F[A1], F[A2], F[A3])): Prod[F] = Prod3[F, A1, A2, A3](p)
  }

  type Cop[F[_]] = Cop3[F, A1, A2, A3]
  object Cop {
    def apply[F[_]](c: (F[A1] \/ (F[A2] \/ F[A3]))): Cop[F] = Cop3[F, A1, A2, A3](c)
  }

  def deriving[TC[_], F[_]](implicit t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose3(t0, t1, t2)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly3(t0, t1, t2)(x => f(Cop3[F, A1, A2, A3](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide3(t0, t1, t2)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply3(t0, t1, t2) {
          case (i0, i1, i2) =>
            f(Prod3[F, A1, A2, A3]((i0, i1, i2)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1], t1: TC[A2], t2: TC[A3]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = deriving[Inj[Prod[F], ?], F].divide
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]] = deriving[Inj[Prod[F], ?], F].choose
    implicit def injProdToVecCopEv[F[_]]: Inj[Vector[Cop[F]], Prod[F]] = deriving[Inj[Vector[Cop[F]], ?], F].divide
  }

}

object AndXor3 {
  def apply[A1, A2, A3]: AndXor3[A1, A2, A3] =
    new AndXor3[A1, A2, A3] {}
}
