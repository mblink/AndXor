package andxor

import andxor.syntax.ffunctor._
import andxor.syntax.ftraverse._
import andxor.types._
import scalaz.{~>, Applicative, Functor, PlusEmpty, Monoid}
import scalaz.Id.Id
import scalaz.std.vector._

trait AndXorNested6[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]]] extends AndXor {

  type Prod[F[_]] = Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]
  object Prod {
    def apply[F[_]](p: (A1[F], A2[F], A3[F], A4[F], A5[F], A6[F])): Prod[F] = Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]](p)
  }

  type Cop[F[_]] = Cop6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]
  object Cop {
    def apply[F[_]](c: Either[A1[F], Either[A2[F], Either[A3[F], Either[A4[F], Either[A5[F], A6[F]]]]]]): Cop[F] = Cop6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]](c)
  }

  object instances {
    implicit def axoProd6Instance(implicit ft0: FTraverse[A1, Applicative], ft1: FTraverse[A2, Applicative], ft2: FTraverse[A3, Applicative], ft3: FTraverse[A4, Applicative], ft4: FTraverse[A5, Applicative], ft5: FTraverse[A6, Applicative]): FFunctor[Prod] with FTraverseProd[Prod] =
      new FFunctor[Prod] with FTraverseProd[Prod] {
        def map[F[_], G[_]](p: Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]])(nt: F ~> G): Prod6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]] =
          Prod6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]]((ft0.map(p.t1)(nt), ft1.map(p.t2)(nt), ft2.map(p.t3)(nt), ft3.map(p.t4)(nt), ft4.map(p.t5)(nt), ft5.map(p.t6)(nt)))

        def traverse[F[_], G[_], A[_]: Applicative](p: Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Prod6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]]] =
          Applicative[A].ap(ft5.traverse(p.t6)(f))(Applicative[A].ap(ft4.traverse(p.t5)(f))(Applicative[A].ap(ft3.traverse(p.t4)(f))(Applicative[A].ap(ft2.traverse(p.t3)(f))(Applicative[A].ap(ft1.traverse(p.t2)(f))(Applicative[A].map(ft0.traverse(p.t1)(f))((i0: A1[G]) => (i1: A2[G]) => (i2: A3[G]) => (i3: A4[G]) => (i4: A5[G]) => (i5: A6[G]) => Prod6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]]((i0, i1, i2, i3, i4, i5))))))))
      }

    implicit def axoProd6FoldMap(implicit fm0: FoldMap[A1, A1], fm1: FoldMap[A2, A2], fm2: FoldMap[A3, A3], fm3: FoldMap[A4, A4], fm4: FoldMap[A5, A5], fm5: FoldMap[A6, A6]): FoldMap[Prod, Cop] =
      new FoldMap[Prod, Cop] {
        def emptyProd[F[_]](implicit PE: PlusEmpty[F]): Prod[F] =
          Prod((fm0.emptyProd, fm1.emptyProd, fm2.emptyProd, fm3.emptyProd, fm4.emptyProd, fm5.emptyProd))

        def unconsAll[F[_], G[_]](p: Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]])(implicit U: Uncons[F, G]): (List[Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]]], Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]) = {
          val (h1, t1) = fm0.unconsAll(p.t1)
          val (h2, t2) = fm1.unconsAll(p.t2)
          val (h3, t3) = fm2.unconsAll(p.t3)
          val (h4, t4) = fm3.unconsAll(p.t4)
          val (h5, t5) = fm4.unconsAll(p.t5)
          val (h6, t6) = fm5.unconsAll(p.t6)
          (
            List(h1.map(Inj[Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]], A1[G]].apply(_)), h2.map(Inj[Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]], A2[G]].apply(_)), h3.map(Inj[Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]], A3[G]].apply(_)), h4.map(Inj[Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]], A4[G]].apply(_)), h5.map(Inj[Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]], A5[G]].apply(_)), h6.map(Inj[Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]], A6[G]].apply(_))).flatten,
            Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]((t1, t2, t3, t4, t5, t6)))
        }

        def unconsOne[F[_], G[_]](p: Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]], c: Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]])(implicit U: Uncons[F, G]): (Option[Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]]], Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]) =
          c.run match {

            case Left(x) =>
              val (h, t) = fm0.unconsOne(p.t1, x)
              (h.map(v => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Left(v))), Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]((t, p.t2, p.t3, p.t4, p.t5, p.t6)))

            case Right(Left(x)) =>
              val (h, t) = fm1.unconsOne(p.t2, x)
              (h.map(v => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Right(Left(v)))), Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]((p.t1, t, p.t3, p.t4, p.t5, p.t6)))

            case Right(Right(Left(x))) =>
              val (h, t) = fm2.unconsOne(p.t3, x)
              (h.map(v => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Right(Right(Left(v))))), Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]((p.t1, p.t2, t, p.t4, p.t5, p.t6)))

            case Right(Right(Right(Left(x)))) =>
              val (h, t) = fm3.unconsOne(p.t4, x)
              (h.map(v => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Right(Right(Right(Left(v)))))), Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]((p.t1, p.t2, p.t3, t, p.t5, p.t6)))

            case Right(Right(Right(Right(Left(x))))) =>
              val (h, t) = fm4.unconsOne(p.t5, x)
              (h.map(v => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Right(Right(Right(Right(Left(v))))))), Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]((p.t1, p.t2, p.t3, p.t4, t, p.t6)))

            case Right(Right(Right(Right(Right(x))))) =>
              val (h, t) = fm5.unconsOne(p.t6, x)
              (h.map(v => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Right(Right(Right(Right(Right(v))))))), Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]((p.t1, p.t2, p.t3, p.t4, p.t5, t)))

          }
      }

    implicit def axoCop6Instance(implicit ft0: FTraverse[A1, Functor], ft1: FTraverse[A2, Functor], ft2: FTraverse[A3, Functor], ft3: FTraverse[A4, Functor], ft4: FTraverse[A5, Functor], ft5: FTraverse[A6, Functor]): FFunctor[Cop] with FTraverseCop[Cop] =
      new FFunctor[Cop] with FTraverseCop[Cop] {
        def map[F[_], G[_]](c: Cop6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]])(nt: F ~> G): Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]] =
          Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](c.run.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.bimap(_.map(nt), _.map(nt)))))))

        def traverse[F[_], G[_], A[_]: Functor](c: Cop6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]])(f: F ~> Lambda[a => A[G[a]]]): A[Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]]] =
          c.run match {

            case Left(x) => Functor[A].map(x.traverse(f))(y => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Left(y)))

            case Right(Left(x)) => Functor[A].map(x.traverse(f))(y => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Right(Left(y))))

            case Right(Right(Left(x))) => Functor[A].map(x.traverse(f))(y => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Right(Right(Left(y)))))

            case Right(Right(Right(Left(x)))) => Functor[A].map(x.traverse(f))(y => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Right(Right(Right(Left(y))))))

            case Right(Right(Right(Right(Left(x))))) => Functor[A].map(x.traverse(f))(y => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Right(Right(Right(Right(Left(y)))))))

            case Right(Right(Right(Right(Right(x))))) => Functor[A].map(x.traverse(f))(y => Cop6[Id, A1[G], A2[G], A3[G], A4[G], A5[G], A6[G]](Right(Right(Right(Right(Right(y)))))))

          }
      }
  }

  def deriving[TC[_], F[_]](implicit t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose6(t0, t1, t2, t3, t4, t5)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly6(t0, t1, t2, t3, t4, t5)(x => f(Cop6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide6(t0, t1, t2, t3, t4, t5)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply6(t0, t1, t2, t3, t4, t5) {
          case (i0, i1, i2, i3, i4, i5) =>
            f(Prod6[Id, A1[F], A2[F], A3[F], A4[F], A5[F], A6[F]]((i0, i1, i2, i3, i4, i5)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1[Id]], t1: TC[A2[Id]], t2: TC[A3[Id]], t3: TC[A4[Id]], t4: TC[A5[Id]], t5: TC[A6[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = deriving[Inj[Prod[F], ?], F].divide
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]] = deriving[Inj[Prod[F], ?], F].choose
    implicit def injProdToVecCopEv[F[_]]: Inj[Vector[Cop[F]], Prod[F]] = deriving[Inj[Vector[Cop[F]], ?], F].divide
  }

}

object AndXorNested6 {
  def apply[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]]]: AndXorNested6[A1, A2, A3, A4, A5, A6] =
    new AndXorNested6[A1, A2, A3, A4, A5, A6] {}
}

trait AndXor6[A1, A2, A3, A4, A5, A6] extends AndXor {

  type Prod[F[_]] = Prod6[F, A1, A2, A3, A4, A5, A6]
  object Prod {
    def apply[F[_]](p: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6])): Prod[F] = Prod6[F, A1, A2, A3, A4, A5, A6](p)
  }

  type Cop[F[_]] = Cop6[F, A1, A2, A3, A4, A5, A6]
  object Cop {
    def apply[F[_]](c: Either[F[A1], Either[F[A2], Either[F[A3], Either[F[A4], Either[F[A5], F[A6]]]]]]): Cop[F] = Cop6[F, A1, A2, A3, A4, A5, A6](c)
  }

  def deriving[TC[_], F[_]](implicit t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        Combine.choose6(t0, t1, t2, t3, t4, t5)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        Combine.altly6(t0, t1, t2, t3, t4, t5)(x => f(Cop6[F, A1, A2, A3, A4, A5, A6](x)))

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        Combine.divide6(t0, t1, t2, t3, t4, t5)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =

        Combine.apply6(t0, t1, t2, t3, t4, t5) {
          case (i0, i1, i2, i3, i4, i5) =>
            f(Prod6[F, A1, A2, A3, A4, A5, A6]((i0, i1, i2, i3, i4, i5)))
        }

    }

  def derivingId[TC[_]](implicit t0: TC[A1], t1: TC[A2], t2: TC[A3], t3: TC[A4], t4: TC[A5], t5: TC[A6]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = deriving[Inj[Prod[F], ?], F].divide
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]] = deriving[Inj[Prod[F], ?], F].choose
    implicit def injProdToVecCopEv[F[_]]: Inj[Vector[Cop[F]], Prod[F]] = deriving[Inj[Vector[Cop[F]], ?], F].divide
  }

}

object AndXor6 {
  def apply[A1, A2, A3, A4, A5, A6]: AndXor6[A1, A2, A3, A4, A5, A6] =
    new AndXor6[A1, A2, A3, A4, A5, A6] {}
}
