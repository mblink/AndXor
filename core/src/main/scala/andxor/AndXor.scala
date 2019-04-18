package andxor

import scalaz.{Apply, Monoid}

abstract class ComposeAndXor[F[_], Cop, Prod] {
  def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B]
  def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B]
  def mkDivide[B](f: B => Prod)(implicit a: Divide[F]): F[B]
  def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B]

  def choose(implicit d: Decidable[F]): F[Cop] = mkChoose(identity _)
  def alt(implicit a: Alt[F]): F[Cop] = mkAlt(identity _)
  def divide(implicit d: Divide[F]): F[Prod] = mkDivide(identity _)
  def apply(implicit a: Apply[F]): F[Prod] = mkApply(identity _)
}

trait AndXor {
  type Cop
  type Prod
  val injEv: Inj[Cop, Cop]
  def inj[A](a: A)(implicit inj: Inj[Cop, A]): Cop = inj(a)
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod]
  def lift[A](a: A)(implicit inj: Inj[Prod, A]): Prod = inj(a)
}

object AndXor {
  def build[A1]: AndXor1[A1] = AndXor1[A1]
  def buildF[A1]: AndXorF1[A1] = AndXorF1[A1]
  def buildK[F[_], A1]: AndXorK1[F, A1] = AndXorK1[F, A1]

  def build[A1, A2]: AndXor2[A1, A2] = AndXor2[A1, A2]
  def buildF[A1, A2]: AndXorF2[A1, A2] = AndXorF2[A1, A2]
  def buildK[F[_], A1, A2]: AndXorK2[F, A1, A2] = AndXorK2[F, A1, A2]

  def build[A1, A2, A3]: AndXor3[A1, A2, A3] = AndXor3[A1, A2, A3]
  def buildF[A1, A2, A3]: AndXorF3[A1, A2, A3] = AndXorF3[A1, A2, A3]
  def buildK[F[_], A1, A2, A3]: AndXorK3[F, A1, A2, A3] = AndXorK3[F, A1, A2, A3]

  def build[A1, A2, A3, A4]: AndXor4[A1, A2, A3, A4] = AndXor4[A1, A2, A3, A4]
  def buildF[A1, A2, A3, A4]: AndXorF4[A1, A2, A3, A4] = AndXorF4[A1, A2, A3, A4]
  def buildK[F[_], A1, A2, A3, A4]: AndXorK4[F, A1, A2, A3, A4] = AndXorK4[F, A1, A2, A3, A4]

  def build[A1, A2, A3, A4, A5]: AndXor5[A1, A2, A3, A4, A5] = AndXor5[A1, A2, A3, A4, A5]
  def buildF[A1, A2, A3, A4, A5]: AndXorF5[A1, A2, A3, A4, A5] = AndXorF5[A1, A2, A3, A4, A5]
  def buildK[F[_], A1, A2, A3, A4, A5]: AndXorK5[F, A1, A2, A3, A4, A5] = AndXorK5[F, A1, A2, A3, A4, A5]

  def build[A1, A2, A3, A4, A5, A6]: AndXor6[A1, A2, A3, A4, A5, A6] = AndXor6[A1, A2, A3, A4, A5, A6]
  def buildF[A1, A2, A3, A4, A5, A6]: AndXorF6[A1, A2, A3, A4, A5, A6] = AndXorF6[A1, A2, A3, A4, A5, A6]
  def buildK[F[_], A1, A2, A3, A4, A5, A6]: AndXorK6[F, A1, A2, A3, A4, A5, A6] = AndXorK6[F, A1, A2, A3, A4, A5, A6]

  def build[A1, A2, A3, A4, A5, A6, A7]: AndXor7[A1, A2, A3, A4, A5, A6, A7] = AndXor7[A1, A2, A3, A4, A5, A6, A7]
  def buildF[A1, A2, A3, A4, A5, A6, A7]: AndXorF7[A1, A2, A3, A4, A5, A6, A7] = AndXorF7[A1, A2, A3, A4, A5, A6, A7]
  def buildK[F[_], A1, A2, A3, A4, A5, A6, A7]: AndXorK7[F, A1, A2, A3, A4, A5, A6, A7] = AndXorK7[F, A1, A2, A3, A4, A5, A6, A7]

  def build[A1, A2, A3, A4, A5, A6, A7, A8]: AndXor8[A1, A2, A3, A4, A5, A6, A7, A8] = AndXor8[A1, A2, A3, A4, A5, A6, A7, A8]
  def buildF[A1, A2, A3, A4, A5, A6, A7, A8]: AndXorF8[A1, A2, A3, A4, A5, A6, A7, A8] = AndXorF8[A1, A2, A3, A4, A5, A6, A7, A8]
  def buildK[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: AndXorK8[F, A1, A2, A3, A4, A5, A6, A7, A8] = AndXorK8[F, A1, A2, A3, A4, A5, A6, A7, A8]

  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9]: AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9] = AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9]
  def buildF[A1, A2, A3, A4, A5, A6, A7, A8, A9]: AndXorF9[A1, A2, A3, A4, A5, A6, A7, A8, A9] = AndXorF9[A1, A2, A3, A4, A5, A6, A7, A8, A9]
  def buildK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: AndXorK9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] = AndXorK9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]

  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]
  def buildF[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: AndXorF10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = AndXorF10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]
  def buildK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: AndXorK10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = AndXorK10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]
  def buildF[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: AndXorF11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = AndXorF11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]
  def buildK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: AndXorK11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = AndXorK11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]
  def buildF[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: AndXorF12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = AndXorF12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]
  def buildK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: AndXorK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = AndXorK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]
  def buildF[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: AndXorF13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
    AndXorF13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]
  def buildK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: AndXorK13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
    AndXorK13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
    AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]
  def buildF[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: AndXorF14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
    AndXorF14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]
  def buildK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: AndXorK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
    AndXorK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] =
    AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
  def buildF[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: AndXorF15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] =
    AndXorF15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
  def buildK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: AndXorK15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] =
    AndXorK15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: AndXor16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] =
    AndXor16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
  def buildF[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: AndXorF16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] =
    AndXorF16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
  def buildK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: AndXorK16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] =
    AndXorK16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: AndXor17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
    AndXor17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
  def buildF[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: AndXorF17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
    AndXorF17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
  def buildK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: AndXorK17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
    AndXorK17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
    AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
  def buildF[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: AndXorF18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
    AndXorF18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
  def buildK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: AndXorK18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
    AndXorK18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
    AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
  def buildF[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: AndXorF19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
    AndXorF19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
  def buildK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
      : AndXorK19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
    AndXorK19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
      : AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
    AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
  def buildF[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
      : AndXorF20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
    AndXorF20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
  def buildK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
      : AndXorK20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
    AndXorK20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
      : AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
    AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
  def buildF[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
      : AndXorF21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
    AndXorF21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
  def buildK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
      : AndXorK21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
    AndXorK21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

  def build[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
      : AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
    AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
  def buildF[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
      : AndXorF22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
    AndXorF22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
  def buildK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
      : AndXorK22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
    AndXorK22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

}
