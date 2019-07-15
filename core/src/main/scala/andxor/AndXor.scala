package andxor

import scalaz.{Apply, Lens, Monoid, PLens}
import scalaz.Id.Id

trait AndXorEvidence[Cop[_[_]], Prod[_[_]]] {
  implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]]
  implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]]
  implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]]

  implicit def injProdToVecCopEvHelper[F[_], A](implicit i: Inj[Cop[F], F[A]]): Inj[Vector[Cop[F]], F[A]] =
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

  def apply[A1, A2]: AndXor2[A1, A2] = AndXor2[A1, A2]

  def apply[A1, A2, A3]: AndXor3[A1, A2, A3] = AndXor3[A1, A2, A3]

  def apply[A1, A2, A3, A4]: AndXor4[A1, A2, A3, A4] = AndXor4[A1, A2, A3, A4]

  def apply[A1, A2, A3, A4, A5]: AndXor5[A1, A2, A3, A4, A5] = AndXor5[A1, A2, A3, A4, A5]

  def apply[A1, A2, A3, A4, A5, A6]: AndXor6[A1, A2, A3, A4, A5, A6] = AndXor6[A1, A2, A3, A4, A5, A6]

  def apply[A1, A2, A3, A4, A5, A6, A7]: AndXor7[A1, A2, A3, A4, A5, A6, A7] = AndXor7[A1, A2, A3, A4, A5, A6, A7]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8]: AndXor8[A1, A2, A3, A4, A5, A6, A7, A8] = AndXor8[A1, A2, A3, A4, A5, A6, A7, A8]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9]: AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9] = AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: AndXor16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = AndXor16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: AndXor17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = AndXor17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

}
