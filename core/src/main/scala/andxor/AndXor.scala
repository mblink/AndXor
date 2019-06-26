package andxor

import scalaz.{Apply, Lens, Monoid, PLens}
import scalaz.Id.Id

trait AndXorEvidence[Cop[_[_]], Prod[_[_]]] {
  implicit def injEv[F[_]]: FInj[Cop, Cop, F]
  implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): FInj[Prod, Prod, F]
  implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): FInj[Prod, Cop, F]

  implicit def injProdToVecCopEvHelper[A[_[_]], F[_]](implicit i: FInj[Cop, A, F]): FInj[Lambda[f[_] => Vector[Cop[f]]], A, F] =
    Inj.instance(a => Vector(i(a)))
  implicit def injProdToVecCopEv[F[_]]: FInj[Lambda[f[_] => Vector[Cop[f]]], Prod, F]
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

  def apply[A1, A2]: AndXorNested2[FConst[A1]#T, FConst[A2]#T] = AndXorNested2[FConst[A1]#T, FConst[A2]#T]
  def nest[A1[_[_]], A2[_[_]]]: AndXorNested2[A1, A2] = AndXorNested2[A1, A2]

  def apply[A1, A2, A3]: AndXorNested3[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T] = AndXorNested3[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]]]: AndXorNested3[A1, A2, A3] = AndXorNested3[A1, A2, A3]

  def apply[A1, A2, A3, A4]: AndXorNested4[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T] = AndXorNested4[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]]]: AndXorNested4[A1, A2, A3, A4] = AndXorNested4[A1, A2, A3, A4]

  def apply[A1, A2, A3, A4, A5]: AndXorNested5[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T] = AndXorNested5[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]]]: AndXorNested5[A1, A2, A3, A4, A5] = AndXorNested5[A1, A2, A3, A4, A5]

  def apply[A1, A2, A3, A4, A5, A6]: AndXorNested6[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T] = AndXorNested6[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]]]: AndXorNested6[A1, A2, A3, A4, A5, A6] = AndXorNested6[A1, A2, A3, A4, A5, A6]

  def apply[A1, A2, A3, A4, A5, A6, A7]: AndXorNested7[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T] = AndXorNested7[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]]]: AndXorNested7[A1, A2, A3, A4, A5, A6, A7] = AndXorNested7[A1, A2, A3, A4, A5, A6, A7]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8]: AndXorNested8[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T] = AndXorNested8[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]]]: AndXorNested8[A1, A2, A3, A4, A5, A6, A7, A8] = AndXorNested8[A1, A2, A3, A4, A5, A6, A7, A8]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9]: AndXorNested9[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T] = AndXorNested9[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]]]: AndXorNested9[A1, A2, A3, A4, A5, A6, A7, A8, A9] = AndXorNested9[A1, A2, A3, A4, A5, A6, A7, A8, A9]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: AndXorNested10[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T] = AndXorNested10[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]]]: AndXorNested10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = AndXorNested10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: AndXorNested11[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T] = AndXorNested11[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]]]: AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: AndXorNested12[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T] = AndXorNested12[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]]]: AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: AndXorNested13[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T] = AndXorNested13[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]]]: AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: AndXorNested14[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T] = AndXorNested14[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]]]: AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: AndXorNested15[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T, FConst[A15]#T] = AndXorNested15[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T, FConst[A15]#T]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]]]: AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: AndXorNested16[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T, FConst[A15]#T, FConst[A16]#T] = AndXorNested16[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T, FConst[A15]#T, FConst[A16]#T]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]]]: AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: AndXorNested17[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T, FConst[A15]#T, FConst[A16]#T, FConst[A17]#T] = AndXorNested17[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T, FConst[A15]#T, FConst[A16]#T, FConst[A17]#T]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]]]: AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: AndXorNested18[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T, FConst[A15]#T, FConst[A16]#T, FConst[A17]#T, FConst[A18]#T] = AndXorNested18[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T, FConst[A15]#T, FConst[A16]#T, FConst[A17]#T, FConst[A18]#T]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]]]: AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: AndXorNested19[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T, FConst[A15]#T, FConst[A16]#T, FConst[A17]#T, FConst[A18]#T, FConst[A19]#T] = AndXorNested19[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T, FConst[A15]#T, FConst[A16]#T, FConst[A17]#T, FConst[A18]#T, FConst[A19]#T]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]]]: AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: AndXorNested20[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T, FConst[A15]#T, FConst[A16]#T, FConst[A17]#T, FConst[A18]#T, FConst[A19]#T, FConst[A20]#T] = AndXorNested20[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T, FConst[A15]#T, FConst[A16]#T, FConst[A17]#T, FConst[A18]#T, FConst[A19]#T, FConst[A20]#T]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]]]: AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: AndXorNested21[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T, FConst[A15]#T, FConst[A16]#T, FConst[A17]#T, FConst[A18]#T, FConst[A19]#T, FConst[A20]#T, FConst[A21]#T] = AndXorNested21[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T, FConst[A15]#T, FConst[A16]#T, FConst[A17]#T, FConst[A18]#T, FConst[A19]#T, FConst[A20]#T, FConst[A21]#T]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]]]: AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: AndXorNested22[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T, FConst[A15]#T, FConst[A16]#T, FConst[A17]#T, FConst[A18]#T, FConst[A19]#T, FConst[A20]#T, FConst[A21]#T, FConst[A22]#T] = AndXorNested22[FConst[A1]#T, FConst[A2]#T, FConst[A3]#T, FConst[A4]#T, FConst[A5]#T, FConst[A6]#T, FConst[A7]#T, FConst[A8]#T, FConst[A9]#T, FConst[A10]#T, FConst[A11]#T, FConst[A12]#T, FConst[A13]#T, FConst[A14]#T, FConst[A15]#T, FConst[A16]#T, FConst[A17]#T, FConst[A18]#T, FConst[A19]#T, FConst[A20]#T, FConst[A21]#T, FConst[A22]#T]
  def nest[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]], A22[_[_]]]: AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

}
