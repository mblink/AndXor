package andxor

import scalaz.{Apply, Monoid}
import scalaz.Id.Id

trait AndXorEvidence[Cop[_[_]], Prod[_[_]]] {
  implicit def injEv[F[_]](implicit d: DerivingCop[Cop, F, Inj[Cop[F], ?]]): Inj[Cop[F], Cop[F]]
  implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]], d: DerivingProd[Prod, F, Inj[Prod[F], ?]]): Inj[Prod[F], Prod[F]]
}

trait AndXor {
  type Cop[F[_]]
  type Prod[F[_]]

  val evidence: AndXorEvidence[Cop, Prod]

  def inj[F[_], A](a: A)(implicit inj: Inj[Cop[F], A]): Cop[F] = inj(a)
  def injId[A](a: A)(implicit inj: Inj[Cop[Id], Id[A]]): Cop[Id] = inj(a)
  def lift[F[_], A](a: A)(implicit inj: Inj[Prod[F], A]): Prod[F] = inj(a)
  def liftId[A](a: A)(implicit inj: Inj[Prod[Id], Id[A]]): Prod[Id] = inj(a)
  def extractC[F[_], B](c: Cop[F])(implicit inj: Inj[Option[F[B]], Cop[F]]): Option[F[B]] = inj(c)
  def extractP[F[_], B](p: Prod[F])(implicit inj: Inj[F[B], Prod[F]]): F[B] = inj(p)

  def choose[TC[_], F[_]](implicit x: Decidable[TC], d: DerivingCop[Cop, F, TC]): TC[Cop[F]] = d.choose
  def alt[TC[_], F[_]](implicit x: Alt[TC], d: DerivingCop[Cop, F, TC]): TC[Cop[F]] = d.alt
  def divide[TC[_], F[_]](implicit x: Divide[TC], d: DerivingProd[Prod, F, TC]): TC[Prod[F]] = d.divide
  def apply[TC[_], F[_]](implicit x: Apply[TC], d: DerivingProd[Prod, F, TC]): TC[Prod[F]] = d.apply

  def choose[TC[_]](implicit x: Decidable[TC], d: DerivingCop[Cop, Id, TC], _d: DummyImplicit): TC[Cop[Id]] = d.choose
  def alt[TC[_]](implicit x: Alt[TC], d: DerivingCop[Cop, Id, TC], _d: DummyImplicit): TC[Cop[Id]] = d.alt
  def divide[TC[_]](implicit x: Divide[TC], d: DerivingProd[Prod, Id, TC], _d: DummyImplicit): TC[Prod[Id]] = d.divide
  def apply[TC[_]](implicit x: Apply[TC], d: DerivingProd[Prod, Id, TC], _d: DummyImplicit): TC[Prod[Id]] = d.apply
}

trait AndXorLift[A] { type AXO <: AndXor }
trait AndXorLiftLP {
  implicit def lp[A]: AndXorLift.Aux[A, AndXor1[A]] = new AndXorLift[A] { type AXO = AndXor1[A] }
}
object AndXorLift extends AndXorLiftLP {
  type Aux[A, AXO0] = AndXorLift[A] { type AXO = AXO0 }

  implicit def hp[A <: AndXor]: AndXorLift.Aux[A, A] = new AndXorLift[A] { type AXO = A }
}

object AndXor {
  def build[A]: AndXor1[A] = AndXor1[A]
  def build[A1, A2](implicit l0: AndXorLift[A1], l1: AndXorLift[A2]): AndXor2[l0.AXO, l1.AXO] = AndXor2[l0.AXO, l1.AXO]
  def build[A1, A2, A3](implicit l0: AndXorLift[A1], l1: AndXorLift[A2], l2: AndXorLift[A3]): AndXor3[l0.AXO, l1.AXO, l2.AXO] = AndXor3[l0.AXO, l1.AXO, l2.AXO]
  def build[A1, A2, A3, A4](implicit l0: AndXorLift[A1], l1: AndXorLift[A2], l2: AndXorLift[A3], l3: AndXorLift[A4]): AndXor4[l0.AXO, l1.AXO, l2.AXO, l3.AXO] = AndXor4[l0.AXO, l1.AXO, l2.AXO, l3.AXO]
  def build[A1, A2, A3, A4, A5](implicit l0: AndXorLift[A1], l1: AndXorLift[A2], l2: AndXorLift[A3], l3: AndXorLift[A4], l4: AndXorLift[A5]): AndXor5[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO] = AndXor5[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO]
  def build[A1, A2, A3, A4, A5, A6](implicit l0: AndXorLift[A1], l1: AndXorLift[A2], l2: AndXorLift[A3], l3: AndXorLift[A4], l4: AndXorLift[A5], l5: AndXorLift[A6]): AndXor6[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO] = AndXor6[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO]
  def build[A1, A2, A3, A4, A5, A6, A7](implicit l0: AndXorLift[A1], l1: AndXorLift[A2], l2: AndXorLift[A3], l3: AndXorLift[A4], l4: AndXorLift[A5], l5: AndXorLift[A6], l6: AndXorLift[A7]): AndXor7[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO] = AndXor7[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO]
  def build[A1, A2, A3, A4, A5, A6, A7, A8](implicit l0: AndXorLift[A1], l1: AndXorLift[A2], l2: AndXorLift[A3], l3: AndXorLift[A4], l4: AndXorLift[A5], l5: AndXorLift[A6], l6: AndXorLift[A7], l7: AndXorLift[A8]): AndXor8[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO] = AndXor8[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit l0: AndXorLift[A1], l1: AndXorLift[A2], l2: AndXorLift[A3], l3: AndXorLift[A4], l4: AndXorLift[A5], l5: AndXorLift[A6], l6: AndXorLift[A7], l7: AndXorLift[A8], l8: AndXorLift[A9]): AndXor9[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO] = AndXor9[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit l0: AndXorLift[A1], l1: AndXorLift[A2], l2: AndXorLift[A3], l3: AndXorLift[A4], l4: AndXorLift[A5], l5: AndXorLift[A6], l6: AndXorLift[A7], l7: AndXorLift[A8], l8: AndXorLift[A9], l9: AndXorLift[A10]): AndXor10[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO] = AndXor10[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit l0: AndXorLift[A1], l1: AndXorLift[A2], l2: AndXorLift[A3], l3: AndXorLift[A4], l4: AndXorLift[A5], l5: AndXorLift[A6], l6: AndXorLift[A7], l7: AndXorLift[A8], l8: AndXorLift[A9], l9: AndXorLift[A10], l10: AndXorLift[A11]): AndXor11[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO] = AndXor11[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit l0: AndXorLift[A1], l1: AndXorLift[A2], l2: AndXorLift[A3], l3: AndXorLift[A4], l4: AndXorLift[A5], l5: AndXorLift[A6], l6: AndXorLift[A7], l7: AndXorLift[A8], l8: AndXorLift[A9], l9: AndXorLift[A10], l10: AndXorLift[A11], l11: AndXorLift[A12]): AndXor12[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO] = AndXor12[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit l0: AndXorLift[A1], l1: AndXorLift[A2], l2: AndXorLift[A3], l3: AndXorLift[A4], l4: AndXorLift[A5], l5: AndXorLift[A6], l6: AndXorLift[A7], l7: AndXorLift[A8], l8: AndXorLift[A9], l9: AndXorLift[A10], l10: AndXorLift[A11], l11: AndXorLift[A12], l12: AndXorLift[A13]): AndXor13[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO, l12.AXO] = AndXor13[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO, l12.AXO]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit l0: AndXorLift[A1], l1: AndXorLift[A2], l2: AndXorLift[A3], l3: AndXorLift[A4], l4: AndXorLift[A5], l5: AndXorLift[A6], l6: AndXorLift[A7], l7: AndXorLift[A8], l8: AndXorLift[A9], l9: AndXorLift[A10], l10: AndXorLift[A11], l11: AndXorLift[A12], l12: AndXorLift[A13], l13: AndXorLift[A14]): AndXor14[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO, l12.AXO, l13.AXO] = AndXor14[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO, l12.AXO, l13.AXO]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit l0: AndXorLift[A1], l1: AndXorLift[A2], l2: AndXorLift[A3], l3: AndXorLift[A4], l4: AndXorLift[A5], l5: AndXorLift[A6], l6: AndXorLift[A7], l7: AndXorLift[A8], l8: AndXorLift[A9], l9: AndXorLift[A10], l10: AndXorLift[A11], l11: AndXorLift[A12], l12: AndXorLift[A13], l13: AndXorLift[A14], l14: AndXorLift[A15]): AndXor15[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO, l12.AXO, l13.AXO, l14.AXO] = AndXor15[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO, l12.AXO, l13.AXO, l14.AXO]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit l0: AndXorLift[A1], l1: AndXorLift[A2], l2: AndXorLift[A3], l3: AndXorLift[A4], l4: AndXorLift[A5], l5: AndXorLift[A6], l6: AndXorLift[A7], l7: AndXorLift[A8], l8: AndXorLift[A9], l9: AndXorLift[A10], l10: AndXorLift[A11], l11: AndXorLift[A12], l12: AndXorLift[A13], l13: AndXorLift[A14], l14: AndXorLift[A15], l15: AndXorLift[A16]): AndXor16[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO, l12.AXO, l13.AXO, l14.AXO, l15.AXO] = AndXor16[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO, l12.AXO, l13.AXO, l14.AXO, l15.AXO]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit l0: AndXorLift[A1], l1: AndXorLift[A2], l2: AndXorLift[A3], l3: AndXorLift[A4], l4: AndXorLift[A5], l5: AndXorLift[A6], l6: AndXorLift[A7], l7: AndXorLift[A8], l8: AndXorLift[A9], l9: AndXorLift[A10], l10: AndXorLift[A11], l11: AndXorLift[A12], l12: AndXorLift[A13], l13: AndXorLift[A14], l14: AndXorLift[A15], l15: AndXorLift[A16], l16: AndXorLift[A17]): AndXor17[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO, l12.AXO, l13.AXO, l14.AXO, l15.AXO, l16.AXO] = AndXor17[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO, l12.AXO, l13.AXO, l14.AXO, l15.AXO, l16.AXO]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit l0: AndXorLift[A1], l1: AndXorLift[A2], l2: AndXorLift[A3], l3: AndXorLift[A4], l4: AndXorLift[A5], l5: AndXorLift[A6], l6: AndXorLift[A7], l7: AndXorLift[A8], l8: AndXorLift[A9], l9: AndXorLift[A10], l10: AndXorLift[A11], l11: AndXorLift[A12], l12: AndXorLift[A13], l13: AndXorLift[A14], l14: AndXorLift[A15], l15: AndXorLift[A16], l16: AndXorLift[A17], l17: AndXorLift[A18]): AndXor18[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO, l12.AXO, l13.AXO, l14.AXO, l15.AXO, l16.AXO, l17.AXO] = AndXor18[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO, l12.AXO, l13.AXO, l14.AXO, l15.AXO, l16.AXO, l17.AXO]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit l0: AndXorLift[A1], l1: AndXorLift[A2], l2: AndXorLift[A3], l3: AndXorLift[A4], l4: AndXorLift[A5], l5: AndXorLift[A6], l6: AndXorLift[A7], l7: AndXorLift[A8], l8: AndXorLift[A9], l9: AndXorLift[A10], l10: AndXorLift[A11], l11: AndXorLift[A12], l12: AndXorLift[A13], l13: AndXorLift[A14], l14: AndXorLift[A15], l15: AndXorLift[A16], l16: AndXorLift[A17], l17: AndXorLift[A18], l18: AndXorLift[A19]): AndXor19[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO, l12.AXO, l13.AXO, l14.AXO, l15.AXO, l16.AXO, l17.AXO, l18.AXO] = AndXor19[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO, l12.AXO, l13.AXO, l14.AXO, l15.AXO, l16.AXO, l17.AXO, l18.AXO]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit l0: AndXorLift[A1], l1: AndXorLift[A2], l2: AndXorLift[A3], l3: AndXorLift[A4], l4: AndXorLift[A5], l5: AndXorLift[A6], l6: AndXorLift[A7], l7: AndXorLift[A8], l8: AndXorLift[A9], l9: AndXorLift[A10], l10: AndXorLift[A11], l11: AndXorLift[A12], l12: AndXorLift[A13], l13: AndXorLift[A14], l14: AndXorLift[A15], l15: AndXorLift[A16], l16: AndXorLift[A17], l17: AndXorLift[A18], l18: AndXorLift[A19], l19: AndXorLift[A20]): AndXor20[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO, l12.AXO, l13.AXO, l14.AXO, l15.AXO, l16.AXO, l17.AXO, l18.AXO, l19.AXO] = AndXor20[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO, l12.AXO, l13.AXO, l14.AXO, l15.AXO, l16.AXO, l17.AXO, l18.AXO, l19.AXO]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit l0: AndXorLift[A1], l1: AndXorLift[A2], l2: AndXorLift[A3], l3: AndXorLift[A4], l4: AndXorLift[A5], l5: AndXorLift[A6], l6: AndXorLift[A7], l7: AndXorLift[A8], l8: AndXorLift[A9], l9: AndXorLift[A10], l10: AndXorLift[A11], l11: AndXorLift[A12], l12: AndXorLift[A13], l13: AndXorLift[A14], l14: AndXorLift[A15], l15: AndXorLift[A16], l16: AndXorLift[A17], l17: AndXorLift[A18], l18: AndXorLift[A19], l19: AndXorLift[A20], l20: AndXorLift[A21]): AndXor21[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO, l12.AXO, l13.AXO, l14.AXO, l15.AXO, l16.AXO, l17.AXO, l18.AXO, l19.AXO, l20.AXO] = AndXor21[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO, l12.AXO, l13.AXO, l14.AXO, l15.AXO, l16.AXO, l17.AXO, l18.AXO, l19.AXO, l20.AXO]
  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit l0: AndXorLift[A1], l1: AndXorLift[A2], l2: AndXorLift[A3], l3: AndXorLift[A4], l4: AndXorLift[A5], l5: AndXorLift[A6], l6: AndXorLift[A7], l7: AndXorLift[A8], l8: AndXorLift[A9], l9: AndXorLift[A10], l10: AndXorLift[A11], l11: AndXorLift[A12], l12: AndXorLift[A13], l13: AndXorLift[A14], l14: AndXorLift[A15], l15: AndXorLift[A16], l16: AndXorLift[A17], l17: AndXorLift[A18], l18: AndXorLift[A19], l19: AndXorLift[A20], l20: AndXorLift[A21], l21: AndXorLift[A22]): AndXor22[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO, l12.AXO, l13.AXO, l14.AXO, l15.AXO, l16.AXO, l17.AXO, l18.AXO, l19.AXO, l20.AXO, l21.AXO] = AndXor22[l0.AXO, l1.AXO, l2.AXO, l3.AXO, l4.AXO, l5.AXO, l6.AXO, l7.AXO, l8.AXO, l9.AXO, l10.AXO, l11.AXO, l12.AXO, l13.AXO, l14.AXO, l15.AXO, l16.AXO, l17.AXO, l18.AXO, l19.AXO, l20.AXO, l21.AXO]
}
