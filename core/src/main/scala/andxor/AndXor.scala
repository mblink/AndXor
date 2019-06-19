package andxor

import scalaz.{Apply, Lens, Monoid, PLens}
import scalaz.Id.Id

trait AndXorEvidence[Cop[_[_]], Prod[_[_]]] {
  implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]]
  implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]]
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

  def fix[F[_]]: AndXorFixed[F] = new AndXorFixed[F] {
    type AXO = self.type
    val axo: self.type = self
    val evidence: AndXorEvidence[self.Cop, self.Prod] = self.evidence
  }

  def inj[F[_], A](a: A)(implicit inj: Inj[Cop[F], A]): Cop[F] = inj(a)
  def injId[A](a: A)(implicit inj: Inj[Cop[Id], Id[A]]): Cop[Id] = inj(a)
  def lift[F[_], A](a: A)(implicit inj: Inj[Prod[F], A]): Prod[F] = inj(a)
  def liftId[A](a: A)(implicit inj: Inj[Prod[Id], Id[A]]): Prod[Id] = inj(a)
  def extractC[F[_], B](c: Cop[F])(implicit l: PLens[Cop[F], B]): Option[B] = l.get(c)
  def extractP[F[_], B](p: Prod[F])(implicit l: Lens[Prod[F], B]): B = l.get(p)

  def derivingNested[TC[_], F[_]](implicit dc: DerivingCop[Cop, F, TC], dp: DerivingProd[Prod, F, TC]): AndXorDeriving[TC, Cop[F], Prod[F]] =
    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[TC]): TC[B] = dc.mkChoose(f)
      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[TC]): TC[B] = dc.mkAlt(f)
      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] = dp.mkDivide(f)
      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] = dp.mkApply(f)
    }
}

trait AndXorFixed[F[_]] {
  type AXO <: AndXor
  val axo: AXO

  type Cop = axo.Cop[F]
  type Prod = axo.Prod[F]

  val evidence: AndXorEvidence[axo.Cop, axo.Prod]

  def inj[A](a: A)(implicit inj: Inj[Cop, A]): Cop = inj(a)
  def lift[A](a: A)(implicit inj: Inj[Prod, A]): Prod = inj(a)
  def extractC[B](c: Cop)(implicit l: PLens[Cop, B]): Option[B] = l.get(c)
  def extractP[B](p: Prod)(implicit l: Lens[Prod, B]): B = l.get(p)
}

sealed trait AndXorLift[A] { type AXO[F[_]] }
trait AndXorConst[A] extends AndXorLift[A] { type AXO[F[_]] = F[A] }
trait AndXorRank2[T[_[_]]] extends AndXorLift[T[Any]] { type AXO[F[_]] = T[F] }

trait AndXorLiftLP {
  type FConst[F[_], A] = F[A]

  implicit def const[A]: AndXorConst[A] = new AndXorConst[A] {}
}
object AndXorLift extends AndXorLiftLP {
  type Aux[A, AXO0[_[_]]] = AndXorLift[A] { type AXO[F[_]] = AXO0[F] }

  implicit def rank2[T[_[_]]]: AndXorRank2[T] = new AndXorRank2[T] {}
}

object AndXor {
  def build[A]: AndXor1[A] = new AndXor1[A] {}
  def buildNested[A[_[_]]]: AndXorNested1[A] = new AndXorNested1[A] {}
  def build[A1, A2]: AndXor2[A1, A2] = AndXor2[A1, A2]
  def buildNested[A1[_[_]], A2[_[_]]]: AndXorNested2[A1, A2] = AndXorNested2[A1, A2]
  def build[A1, A2, A3]: AndXor3[A1, A2, A3] = AndXor3[A1, A2, A3]
  def buildNested[A1[_[_]], A2[_[_]], A3[_[_]]]: AndXorNested3[A1, A2, A3] = AndXorNested3[A1, A2, A3]
  def build[A1, A2, A3, A4]: AndXor4[A1, A2, A3, A4] = AndXor4[A1, A2, A3, A4]
  def buildNested[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]]]: AndXorNested4[A1, A2, A3, A4] = AndXorNested4[A1, A2, A3, A4]
  def build[A1, A2, A3, A4, A5]: AndXor5[A1, A2, A3, A4, A5] = AndXor5[A1, A2, A3, A4, A5]
  def buildNested[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]]]: AndXorNested5[A1, A2, A3, A4, A5] = AndXorNested5[A1, A2, A3, A4, A5]
  def build[A1, A2, A3, A4, A5, A6]: AndXor6[A1, A2, A3, A4, A5, A6] = AndXor6[A1, A2, A3, A4, A5, A6]
  def buildNested[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]]]: AndXorNested6[A1, A2, A3, A4, A5, A6] = AndXorNested6[A1, A2, A3, A4, A5, A6]
  def build[A1, A2, A3, A4, A5, A6, A7]: AndXor7[A1, A2, A3, A4, A5, A6, A7] = AndXor7[A1, A2, A3, A4, A5, A6, A7]
  def buildNested[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]]]: AndXorNested7[A1, A2, A3, A4, A5, A6, A7] = AndXorNested7[A1, A2, A3, A4, A5, A6, A7]
  def build[A1, A2, A3, A4, A5, A6, A7, A8]: AndXor8[A1, A2, A3, A4, A5, A6, A7, A8] = AndXor8[A1, A2, A3, A4, A5, A6, A7, A8]
  def buildNested[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]]]: AndXorNested8[A1, A2, A3, A4, A5, A6, A7, A8] = AndXorNested8[A1, A2, A3, A4, A5, A6, A7, A8]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9]: AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9] = AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9]
  def buildNested[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]]]: AndXorNested9[A1, A2, A3, A4, A5, A6, A7, A8, A9] = AndXorNested9[A1, A2, A3, A4, A5, A6, A7, A8, A9]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]
  def buildNested[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]]]: AndXorNested10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = AndXorNested10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]
  def buildNested[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]]]: AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]
  def buildNested[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]]]: AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]
  def buildNested[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]]]: AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]
  def buildNested[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]]]: AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
  def buildNested[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]]]: AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: AndXor16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = AndXor16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
  def buildNested[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]]]: AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: AndXor17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = AndXor17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
  def buildNested[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]]]: AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
  def buildNested[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]]]: AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
  def buildNested[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]]]: AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
  def buildNested[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]]]: AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
  def buildNested[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]]]: AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
  def buildNested[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]], A22[_[_]]]: AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
}
