package andxor

import andxor.syntax.ffunctor._
import andxor.syntax.ftraverse._
import andxor.types._
import scalaz.{~>, \/, -\/, \/-, Applicative, Functor, Apply, Monoid}
import scalaz.Id.Id
import scalaz.std.vector._

trait AndXorNested4[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]]] extends AndXor {

  type Prod[F[_]] = Prod4[Id, A1[F], A2[F], A3[F], A4[F]]
  object Prod {
    def apply[F[_]](p: (A1[F], A2[F], A3[F], A4[F])): Prod[F] = Prod4[Id, A1[F], A2[F], A3[F], A4[F]](p)
  }

  type Cop[F[_]] = Cop4[Id, A1[F], A2[F], A3[F], A4[F]]
  object Cop {
    def apply[F[_]](c: (A1[F] \/ (A2[F] \/ (A3[F] \/ A4[F])))): Cop[F] = Cop4[Id, A1[F], A2[F], A3[F], A4[F]](c)
  }

  object instances {
    implicit def axoProd4Instance(implicit ft0: FTraverse[A1], ft1: FTraverse[A2], ft2: FTraverse[A3], ft3: FTraverse[A4]): FFunctor[Prod] with FTraverse[Prod] =
      new FFunctor[Prod] with FTraverse[Prod] {
        def map[F[_], G[_]](p: Prod4[Id, A1[F], A2[F], A3[F], A4[F]])(nt: F ~> G): Prod4[Id, A1[G], A2[G], A3[G], A4[G]] =
          Prod4[Id, A1[G], A2[G], A3[G], A4[G]]((ft0.map(p.t1)(nt), ft1.map(p.t2)(nt), ft2.map(p.t3)(nt), ft3.map(p.t4)(nt)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod4[Id, A1[F], A2[F], A3[F], A4[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Prod4[Id, A1[G], A2[G], A3[G], A4[G]]] =
          Applicative[A].ap(ft3.traverse(p.t4)(f))(Applicative[A].ap(ft2.traverse(p.t3)(f))(Applicative[A].ap(ft1.traverse(p.t2)(f))(Applicative[A].map(ft0.traverse(p.t1)(f))((i0: A1[G]) => (i1: A2[G]) => (i2: A3[G]) => (i3: A4[G]) => Prod4[Id, A1[G], A2[G], A3[G], A4[G]]((i0, i1, i2, i3))))))
      }

    implicit def axoProd4FoldMap(implicit fm0: FoldMap[A1, A1], fm1: FoldMap[A2, A2], fm2: FoldMap[A3, A3], fm3: FoldMap[A4, A4]): FoldMap[Prod, Cop] =
      new FoldMap[Prod, Cop] {
        def unconsAll[F[_], G[_]](p: Prod4[Id, A1[F], A2[F], A3[F], A4[F]])(implicit U: Uncons[F, G]): (List[Cop4[Id, A1[G], A2[G], A3[G], A4[G]]], Prod4[Id, A1[F], A2[F], A3[F], A4[F]]) = {
          val (h1, t1) = fm0.unconsAll(p.t1)
          val (h2, t2) = fm1.unconsAll(p.t2)
          val (h3, t3) = fm2.unconsAll(p.t3)
          val (h4, t4) = fm3.unconsAll(p.t4)
          (
            List(h1.map(Inj[Cop4[Id, A1[G], A2[G], A3[G], A4[G]], A1[G]].apply(_)), h2.map(Inj[Cop4[Id, A1[G], A2[G], A3[G], A4[G]], A2[G]].apply(_)), h3.map(Inj[Cop4[Id, A1[G], A2[G], A3[G], A4[G]], A3[G]].apply(_)), h4.map(Inj[Cop4[Id, A1[G], A2[G], A3[G], A4[G]], A4[G]].apply(_))).flatten,
            Prod4[Id, A1[F], A2[F], A3[F], A4[F]]((t1, t2, t3, t4)))
        }

        def unconsOne[F[_], G[_]](p: Prod4[Id, A1[F], A2[F], A3[F], A4[F]], c: Cop4[Id, A1[G], A2[G], A3[G], A4[G]])(implicit U: Uncons[F, G]): (Option[Cop4[Id, A1[G], A2[G], A3[G], A4[G]]], Prod4[Id, A1[F], A2[F], A3[F], A4[F]]) =
          c.run match {

            case -\/(x) =>
              val (h, t) = fm0.unconsOne(p.t1, x)
              (h.map(v => Cop4[Id, A1[G], A2[G], A3[G], A4[G]](-\/(v))), Prod4[Id, A1[F], A2[F], A3[F], A4[F]]((t, p.t2, p.t3, p.t4)))

            case \/-(-\/(x)) =>
              val (h, t) = fm1.unconsOne(p.t2, x)
              (h.map(v => Cop4[Id, A1[G], A2[G], A3[G], A4[G]](\/-(-\/(v)))), Prod4[Id, A1[F], A2[F], A3[F], A4[F]]((p.t1, t, p.t3, p.t4)))

            case \/-(\/-(-\/(x))) =>
              val (h, t) = fm2.unconsOne(p.t3, x)
              (h.map(v => Cop4[Id, A1[G], A2[G], A3[G], A4[G]](\/-(\/-(-\/(v))))), Prod4[Id, A1[F], A2[F], A3[F], A4[F]]((p.t1, p.t2, t, p.t4)))

            case \/-(\/-(\/-(x))) =>
              val (h, t) = fm3.unconsOne(p.t4, x)
              (h.map(v => Cop4[Id, A1[G], A2[G], A3[G], A4[G]](\/-(\/-(\/-(v))))), Prod4[Id, A1[F], A2[F], A3[F], A4[F]]((p.t1, p.t2, p.t3, t)))

          }
      }

    implicit def axoCop4Instance(implicit ft0: FTraverse[A1], ft1: FTraverse[A2], ft2: FTraverse[A3], ft3: FTraverse[A4]): FFunctor[Cop] with FTraverse[Cop] =
      new FFunctor[Cop] with FTraverse[Cop] {
        def map[F[_], G[_]](c: Cop4[Id, A1[F], A2[F], A3[F], A4[F]])(nt: F ~> G): Cop4[Id, A1[G], A2[G], A3[G], A4[G]] =
          Cop4[Id, A1[G], A2[G], A3[G], A4[G]](c.run.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.map(nt)))))

        def traverse[F[_], G[_], A[_]: Applicative](c: Cop4[Id, A1[F], A2[F], A3[F], A4[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Cop4[Id, A1[G], A2[G], A3[G], A4[G]]] =
          c.run match {

            case -\/(x) => Functor[A].map(x.traverse(f))(y => Cop4[Id, A1[G], A2[G], A3[G], A4[G]](-\/(y)))

            case \/-(-\/(x)) => Functor[A].map(x.traverse(f))(y => Cop4[Id, A1[G], A2[G], A3[G], A4[G]](\/-(-\/(y))))

            case \/-(\/-(-\/(x))) => Functor[A].map(x.traverse(f))(y => Cop4[Id, A1[G], A2[G], A3[G], A4[G]](\/-(\/-(-\/(y)))))

            case \/-(\/-(\/-(x))) => Functor[A].map(x.traverse(f))(y => Cop4[Id, A1[G], A2[G], A3[G], A4[G]](\/-(\/-(\/-(y)))))

          }
      }
  }

  def deriving[TC[_], F[_]](implicit t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose4(t0, t1, t2, t3)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly4(t0, t1, t2, t3)(x => f(Cop4[Id, A1[F], A2[F], A3[F], A4[F]](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide4(t0, t1, t2, t3)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply4(t0, t1, t2, t3) {
          case (i0, i1, i2, i3) =>
            f(Prod4[Id, A1[F], A2[F], A3[F], A4[F]]((i0, i1, i2, i3)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1[Id]], t1: TC[A2[Id]], t2: TC[A3[Id]], t3: TC[A4[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = deriving[Inj[Prod[F], ?], F].divide
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]] = deriving[Inj[Prod[F], ?], F].choose
    implicit def injProdToVecCopEv[F[_]]: Inj[Vector[Cop[F]], Prod[F]] = deriving[Inj[Vector[Cop[F]], ?], F].divide
  }

}

object AndXorNested4 {
  def apply[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]]]: AndXorNested4[A1, A2, A3, A4] =
    new AndXorNested4[A1, A2, A3, A4] {}
}

trait AndXor4[A1, A2, A3, A4] extends AndXor {

  type Prod[F[_]] = Prod4[F, A1, A2, A3, A4]
  object Prod {
    def apply[F[_]](p: (F[A1], F[A2], F[A3], F[A4])): Prod[F] = Prod4[F, A1, A2, A3, A4](p)
  }

  type Cop[F[_]] = Cop4[F, A1, A2, A3, A4]
  object Cop {
    def apply[F[_]](c: (F[A1] \/ (F[A2] \/ (F[A3] \/ F[A4])))): Cop[F] = Cop4[F, A1, A2, A3, A4](c)
  }

  def deriving[TC[_], F[_]](implicit t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose4(t0, t1, t2, t3)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly4(t0, t1, t2, t3)(x => f(Cop4[F, A1, A2, A3, A4](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide4(t0, t1, t2, t3)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply4(t0, t1, t2, t3) {
          case (i0, i1, i2, i3) =>
            f(Prod4[F, A1, A2, A3, A4]((i0, i1, i2, i3)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1], t1: TC[A2], t2: TC[A3], t3: TC[A4]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = deriving[Inj[Prod[F], ?], F].divide
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]] = deriving[Inj[Prod[F], ?], F].choose
    implicit def injProdToVecCopEv[F[_]]: Inj[Vector[Cop[F]], Prod[F]] = deriving[Inj[Vector[Cop[F]], ?], F].divide
  }

}

object AndXor4 {
  def apply[A1, A2, A3, A4]: AndXor4[A1, A2, A3, A4] =
    new AndXor4[A1, A2, A3, A4] {}
}
