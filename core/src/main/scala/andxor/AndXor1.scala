package andxor

import scalaz.{Apply, Monoid}
import scalaz.Id.Id

trait AndXorNested1[A1[_[_]]] extends AndXor {
  type Prod[F[_]] = A1[F]
  object Prod {
    def apply[F[_]](p: A1[F]): Prod[F] = p
  }

  type Cop[F[_]] = A1[F]
  object Cop {
    def apply[F[_]](c: A1[F]): Cop[F] = c
  }

  def deriving[TC[_], F[_]](implicit t0: TC[A1[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =
    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] =
        d.contramap(t0)(f)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] =
        a.map(t0)(f)

      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] =
        a.contramap(t0)(f)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] =
        a.map(t0)(f)
    }

  def derivingId[TC[_]](implicit t0: TC[A1[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] =
      injEv[F]
  }
}

object AndXorNested1 {
  def apply[A1[_[_]]]: AndXorNested1[A1] =
    new AndXorNested1[A1] {}
}

trait AndXor1[A1] extends AndXorNested1[FConst[?[_], A1]] {
  override def derivingId[TC[_]](implicit t0: TC[A1]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]
}

object AndXor1 {
  def apply[A1]: AndXor1[A1] =
    new AndXor1[A1] {}
}
