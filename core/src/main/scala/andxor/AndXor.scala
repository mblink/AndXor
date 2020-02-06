package andxor

import scalaz.{Lens, Monoid, PLens}
import scalaz.Id.Id

trait AndXorEvidence[Cop[_[_]], Prod[_[_]]] {
  implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]]
  implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]]
  implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]]

  implicit def injProdToVecCopEvHelper[F[_], A](implicit i: Inj[Cop[F], F[A]]): Inj[Vector[Cop[F]], F[A]] =
    Inj.instance(a => Vector(i(a)))

  implicit def injNestedProdToVecCopEvHelper[A[_[_]], F[_]](implicit i: Inj[Cop[F], Id[A[F]]]): Inj[Vector[Cop[F]], A[F]] =
    Inj.instance(a => Vector(i(a)))

  implicit def injProdToVecCopEv[F[_]]: Inj[Vector[Cop[F]], Prod[F]]
}

trait AndXorDeriving[TC[_], Cop, Prod] {
  def mkChoose[B](f: B => Cop)(implicit d: Decidable[TC]): TC[B]
  def mkAlt[B](f: Cop => B)(implicit a: Alt[TC]): TC[B]
  def mkDivide[B](f: B => Prod)(implicit a: Divide[TC]): TC[B]
  def mkApply[B](f: Prod => B)(implicit a: Apply[TC]): TC[B]

  def choose(implicit d: Decidable[TC]): TC[Cop] = mkChoose(identity _)
  def alt(implicit a: Alt[TC]): TC[Cop] = mkAlt(identity _)
  def divide(implicit d: Divide[TC]): TC[Prod] = mkDivide(identity _)
  def apply(implicit a: Apply[TC]): TC[Prod] = mkApply(identity _)
}

trait AndXor { self =>
  type Cop[F[_]]
  type Prod[F[_]]

  val evidence: AndXorEvidence[Cop, Prod]

  def inj[F[_], A](a: A)(implicit inj: Inj[Cop[F], A]): Cop[F] = inj(a)
  def injId[A](a: A)(implicit inj: Inj[Cop[Id], Id[A]]): Cop[Id] = inj(a)
  def lift[F[_], A](a: A)(implicit inj: Inj[Prod[F], A]): Prod[F] = inj(a)
  def liftId[A](a: A)(implicit inj: Inj[Prod[Id], Id[A]]): Prod[Id] = inj(a)
  def extractC[F[_], B](c: Cop[F])(implicit l: PLens[Cop[F], B]): Option[B] = l.get(c)
  def extractP[F[_], B](p: Prod[F])(implicit l: Lens[Prod[F], B]): B = l.get(p)

  def derivingNestedCop[TC[_], F[_]](implicit dc: DerivingCop[Cop, F, TC]): DerivingCop[Cop, F, TC] = dc
  def derivingNestedProd[TC[_], F[_]](implicit dp: DerivingProd[Prod, F, TC]): DerivingProd[Prod, F, TC] = dp
}

object AndXor {
  def apply[A]: AndXor1[A] = new AndXor1[A] {}
  def nest[A[_[_]]]: AndXorNested1[A] = new AndXorNested1[A] {}

  def apply[A1, A2]: AndXor2[A1, A2] = AndXor2[A1, A2]
  def nest[A1[_[_]], A2[_[_]]]: AndXorNested2[A1, A2] = AndXorNested2[A1, A2]

  def apply[A1, A2, A3]: AndXor3[A1, A2, A3] = AndXor3[A1, A2, A3]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]]]: AndXorNested3[A1, A2, A3] = AndXorNested3[A1, A2, A3]

  def apply[A1, A2, A3, A4]: AndXor4[A1, A2, A3, A4] = AndXor4[A1, A2, A3, A4]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]]]: AndXorNested4[A1, A2, A3, A4] = AndXorNested4[A1, A2, A3, A4]

  def apply[A1, A2, A3, A4, A5]: AndXor5[A1, A2, A3, A4, A5] = AndXor5[A1, A2, A3, A4, A5]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]]]: AndXorNested5[A1, A2, A3, A4, A5] = AndXorNested5[A1, A2, A3, A4, A5]

  def apply[A1, A2, A3, A4, A5, A6]: AndXor6[A1, A2, A3, A4, A5, A6] = AndXor6[A1, A2, A3, A4, A5, A6]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]]]: AndXorNested6[A1, A2, A3, A4, A5, A6] = AndXorNested6[A1, A2, A3, A4, A5, A6]

}
