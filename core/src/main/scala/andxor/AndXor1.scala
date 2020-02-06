package andxor

import scalaz.{Monoid}
import scalaz.Id.Id
import scalaz.std.vector._

trait AndXorNested1[A1[_[_]]] extends AndXor {

  def apply[B1]: AndXorNested2[A1, FConst[B1]#T] = AndXorNested2[A1, FConst[B1]#T]
  def nest[B1[_[_]]]: AndXorNested2[A1, B1] = AndXorNested2[A1, B1]

  def apply[B1, B2]: AndXorNested3[A1, FConst[B1]#T, FConst[B2]#T] = AndXorNested3[A1, FConst[B1]#T, FConst[B2]#T]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested3[A1, B1, B2] = AndXorNested3[A1, B1, B2]

  def apply[B1, B2, B3]: AndXorNested4[A1, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T] = AndXorNested4[A1, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]]]: AndXorNested4[A1, B1, B2, B3] = AndXorNested4[A1, B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXorNested5[A1, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T] = AndXorNested5[A1, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]]]: AndXorNested5[A1, B1, B2, B3, B4] = AndXorNested5[A1, B1, B2, B3, B4]

  def apply[B1, B2, B3, B4, B5]: AndXorNested6[A1, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T] = AndXorNested6[A1, FConst[B1]#T, FConst[B2]#T, FConst[B3]#T, FConst[B4]#T, FConst[B5]#T]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]]]: AndXorNested6[A1, B1, B2, B3, B4, B5] = AndXorNested6[A1, B1, B2, B3, B4, B5]

  type Prod[F[_]] = Id[A1[F]]
  object Prod {
    def apply[F[_]](p: A1[F]): Prod[F] = p
  }

  type Cop[F[_]] = Id[A1[F]]
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
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = injEv[F]
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]] = injEv[F]
    implicit def injProdToVecCopEv[F[_]]: Inj[Vector[Cop[F]], Prod[F]] = deriving[Inj[Vector[Cop[F]], ?], F].divide
  }

}

object AndXorNested1 {
  def apply[A1[_[_]]]: AndXorNested1[A1] =
    new AndXorNested1[A1] {}
}

trait AndXor1[A1] extends AndXor {

  def apply[B1]: AndXor2[A1, B1] = AndXor2[A1, B1]
  def nest[B1[_[_]]]: AndXorNested2[FConst[A1]#T, B1] = AndXorNested2[FConst[A1]#T, B1]

  def apply[B1, B2]: AndXor3[A1, B1, B2] = AndXor3[A1, B1, B2]
  def nest[B1[_[_]], B2[_[_]]]: AndXorNested3[FConst[A1]#T, B1, B2] = AndXorNested3[FConst[A1]#T, B1, B2]

  def apply[B1, B2, B3]: AndXor4[A1, B1, B2, B3] = AndXor4[A1, B1, B2, B3]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]]]: AndXorNested4[FConst[A1]#T, B1, B2, B3] = AndXorNested4[FConst[A1]#T, B1, B2, B3]

  def apply[B1, B2, B3, B4]: AndXor5[A1, B1, B2, B3, B4] = AndXor5[A1, B1, B2, B3, B4]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]]]: AndXorNested5[FConst[A1]#T, B1, B2, B3, B4] = AndXorNested5[FConst[A1]#T, B1, B2, B3, B4]

  def apply[B1, B2, B3, B4, B5]: AndXor6[A1, B1, B2, B3, B4, B5] = AndXor6[A1, B1, B2, B3, B4, B5]
  def nest[B1[_[_]], B2[_[_]], B3[_[_]], B4[_[_]], B5[_[_]]]: AndXorNested6[FConst[A1]#T, B1, B2, B3, B4, B5] = AndXorNested6[FConst[A1]#T, B1, B2, B3, B4, B5]

  type Prod[F[_]] = F[A1]
  object Prod {
    def apply[F[_]](p: F[A1]): Prod[F] = p
  }

  type Cop[F[_]] = F[A1]
  object Cop {
    def apply[F[_]](c: F[A1]): Cop[F] = c
  }

  def deriving[TC[_], F[_]](implicit t0: TC[F[A1]]): AndXorDeriving[TC, Cop[F], Prod[F]] =

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

  def derivingId[TC[_]](implicit t0: TC[A1]): AndXorDeriving[TC, Cop[Id], Prod[Id]] = deriving[TC, Id]

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = deriving[Inj[Cop[F], ?], F].choose
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = injEv[F]
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]] = injEv[F]
    implicit def injProdToVecCopEv[F[_]]: Inj[Vector[Cop[F]], Prod[F]] = deriving[Inj[Vector[Cop[F]], ?], F].divide
  }

}

object AndXor1 {
  def apply[A1]: AndXor1[A1] =
    new AndXor1[A1] {}
}
