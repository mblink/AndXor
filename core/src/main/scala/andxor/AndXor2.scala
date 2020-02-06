package andxor

import andxor.syntax.ffunctor._
import andxor.syntax.ftraverse._
import andxor.types._
import scalaz.{~>, Applicative, Functor, PlusEmpty, Monoid}
import scalaz.Id.Id
import scalaz.std.vector._

trait AndXorNested2[A1[_[_]], A2[_[_]]] extends AndXor {

  def apply[B1]: AndXorNested3[A1, A2, FConst[B1]#T] = AndXorNested3[A1, A2, FConst[B1]#T]
  def nest[B1[_[_]]]: AndXorNested3[A1, A2, B1] = AndXorNested3[A1, A2, B1]

  def apply[B1, B2]: AndXorNested4[A1, A2, FConst[B1]#T, FConst[B2]#T] = AndXorNested4[A1, A2, FConst[B1]#T, FConst[B2]#T]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested4[A1, A2, B1, B2] = AndXorNested4[A1, A2, B1, B2]

  def apply[B1, B2, B3]: AndXorNested5[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T] = AndXorNested5[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]]]: AndXorNested5[A1, A2, B1, B2, B3] = AndXorNested5[A1, A2, B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXorNested6[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T] = AndXorNested6[A1, A2, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]]]: AndXorNested6[A1, A2, B1, B2, B3, B4] = AndXorNested6[A1, A2, B1, B2, B3, B4]

  type Prod[F[_]] = Prod2[Id, A1[F], A2[F]]
  object Prod {
    def apply[F[_]](p: (A1[F], A2[F])): Prod[F] = Prod2[Id, A1[F], A2[F]](p)
  }

  type Cop[F[_]] = Cop2[Id, A1[F], A2[F]]
  object Cop {
    def apply[F[_]](c: Either[A1[F], A2[F]]): Cop[F] = Cop2[Id, A1[F], A2[F]](c)
  }

  object instances {
    implicit def axoProd2Instance(implicit ft0: FTraverse[A1, Applicative], ft1: FTraverse[A2, Applicative]): FFunctor[Prod] with FTraverseProd[Prod] =
      new FFunctor[Prod] with FTraverseProd[Prod] {
        def map[F[_], G[_]](p: Prod2[Id, A1[F], A2[F]])(nt: F ~> G): Prod2[Id, A1[G], A2[G]] =
          Prod2[Id, A1[G], A2[G]]((ft0.map(p.t1)(nt), ft1.map(p.t2)(nt)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod2[Id, A1[F], A2[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Prod2[Id, A1[G], A2[G]]] =
          Applicative[A].ap(ft1.traverse(p.t2)(f))(Applicative[A].map(ft0.traverse(p.t1)(f))((i0: A1[G]) => (i1: A2[G]) => Prod2[Id, A1[G], A2[G]]((i0, i1))))
      }

    implicit def axoProd2FoldMap(implicit fm0: FoldMap[A1, A1], fm1: FoldMap[A2, A2]): FoldMap[Prod, Cop] =
      new FoldMap[Prod, Cop] {
        def emptyProd[F[_]](implicit PE: PlusEmpty[F]): Prod[F] =
          Prod((fm0.emptyProd, fm1.emptyProd))

        def unconsAll[F[_], G[_]](p: Prod2[Id, A1[F], A2[F]])(implicit U: Uncons[F, G]): (List[Cop2[Id, A1[G], A2[G]]], Prod2[Id, A1[F], A2[F]]) = {
          val (h1, t1) = fm0.unconsAll(p.t1)
          val (h2, t2) = fm1.unconsAll(p.t2)
          (
            List(h1.map(Inj[Cop2[Id, A1[G], A2[G]], A1[G]].apply(_)), h2.map(Inj[Cop2[Id, A1[G], A2[G]], A2[G]].apply(_))).flatten,
            Prod2[Id, A1[F], A2[F]]((t1, t2)))
        }

        def unconsOne[F[_], G[_]](p: Prod2[Id, A1[F], A2[F]], c: Cop2[Id, A1[G], A2[G]])(implicit U: Uncons[F, G]): (Option[Cop2[Id, A1[G], A2[G]]], Prod2[Id, A1[F], A2[F]]) =
          c.run match {

            case Left(x) =>
              val (h, t) = fm0.unconsOne(p.t1, x)
              (h.map(v => Cop2[Id, A1[G], A2[G]](Left(v))), Prod2[Id, A1[F], A2[F]]((t, p.t2)))

            case Right(x) =>
              val (h, t) = fm1.unconsOne(p.t2, x)
              (h.map(v => Cop2[Id, A1[G], A2[G]](Right(v))), Prod2[Id, A1[F], A2[F]]((p.t1, t)))

          }
      }

    implicit def axoCop2Instance(implicit ft0: FTraverse[A1, Functor], ft1: FTraverse[A2, Functor]): FFunctor[Cop] with FTraverseCop[Cop] =
      new FFunctor[Cop] with FTraverseCop[Cop] {
        def map[F[_], G[_]](c: Cop2[Id, A1[F], A2[F]])(nt: F ~> G): Cop2[Id, A1[G], A2[G]] =
          Cop2[Id, A1[G], A2[G]](c.run.bimap(_.map(nt), _.map(nt)))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop2[Id, A1[F], A2[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Cop2[Id, A1[G], A2[G]]] =
          c.run match {

            case Left(x) => Functor[A].map(x.traverse(f))(y => Cop2[Id, A1[G], A2[G]](Left(y)))

            case Right(x) => Functor[A].map(x.traverse(f))(y => Cop2[Id, A1[G], A2[G]](Right(y)))

          }
      }
  }

  def deriving[TC[_], F[_]](implicit t0: TC[A1[F]], t1: TC[A2[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose2(t0, t1)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly2(t0, t1)(x => f(Cop2[Id, A1[F], A2[F]](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide2(t0, t1)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply2(t0, t1) {
          case (i0, i1) =>
            f(Prod2[Id, A1[F], A2[F]]((i0, i1)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1[Id]], t1: TC[A2[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = deriving[Inj[Prod[F], ?], F].divide
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]] = deriving[Inj[Prod[F], ?], F].choose
    implicit def injProdToVecCopEv[F[_]]: Inj[Vector[Cop[F]], Prod[F]] = deriving[Inj[Vector[Cop[F]], ?], F].divide
  }

}

object AndXorNested2 {
  def apply[A1[_[_]], A2[_[_]]]: AndXorNested2[A1, A2] =
    new AndXorNested2[A1, A2] {}
}

trait AndXor2[A1, A2] extends AndXor {

  def apply[B1]: AndXor3[A1, A2, B1] = AndXor3[A1, A2, B1]
  def nest[B1[_[_]]]: AndXorNested3[FConst[A1]#T, FConst[A2]#T, B1] = AndXorNested3[FConst[A1]#T, FConst[A2]#T, B1]

  def apply[B1, B2]: AndXor4[A1, A2, B1, B2] = AndXor4[A1, A2, B1, B2]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested4[FConst[A1]#T, FConst[A2]#T, B1, B2] = AndXorNested4[FConst[A1]#T, FConst[A2]#T, B1, B2]

  def apply[B1, B2, B3]: AndXor5[A1, A2, B1, B2, B3] = AndXor5[A1, A2, B1, B2, B3]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]]]: AndXorNested5[FConst[A1]#T, FConst[A2]#T, B1, B2, B3] = AndXorNested5[FConst[A1]#T, FConst[A2]#T, B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXor6[A1, A2, B1, B2, B3, B4] = AndXor6[A1, A2, B1, B2, B3, B4]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]]]: AndXorNested6[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4] = AndXorNested6[FConst[A1]#T, FConst[A2]#T, B1, B2, B3, B4]

  type Prod[F[_]] = Prod2[F, A1, A2]
  object Prod {
    def apply[F[_]](p: (F[A1], F[A2])): Prod[F] = Prod2[F, A1, A2](p)
  }

  type Cop[F[_]] = Cop2[F, A1, A2]
  object Cop {
    def apply[F[_]](c: Either[F[A1], F[A2]]): Cop[F] = Cop2[F, A1, A2](c)
  }

  def deriving[TC[_], F[_]](implicit t0: TC[F[A1]], t1: TC[F[A2]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose2(t0, t1)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly2(t0, t1)(x => f(Cop2[F, A1, A2](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide2(t0, t1)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply2(t0, t1) {
          case (i0, i1) =>
            f(Prod2[F, A1, A2]((i0, i1)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1], t1: TC[A2]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = deriving[Inj[Prod[F], ?], F].divide
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]] = deriving[Inj[Prod[F], ?], F].choose
    implicit def injProdToVecCopEv[F[_]]: Inj[Vector[Cop[F]], Prod[F]] = deriving[Inj[Vector[Cop[F]], ?], F].divide
  }

}

object AndXor2 {
  def apply[A1, A2]: AndXor2[A1, A2] =
    new AndXor2[A1, A2] {}
}
