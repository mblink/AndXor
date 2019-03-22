package andxor

import andxor.tuple._
import scala.language.higherKinds
import scalaz.{Apply, Functor, PlusEmpty, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id

trait AndXor23[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23] extends AndXor {
  case class ProdT[F[_]](
      run: (
          F[A1],
          (
              F[A2],
              (
                  F[A3],
                  (
                      F[A4],
                      (
                          F[A5],
                          (F[A6], (F[A7], (F[A8], (F[A9], (F[A10], (F[A11], (F[A12], (F[A13], (F[A14], (F[A15], (F[A16], (F[A17], (F[A18], (F[A19], (F[A20], (F[A21], (F[A22], F[A23])))))))))))))))))
                      )
                  )
              )
          )
      )
  )
  object ProdT {

    implicit def lifta0[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A1]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              x,
              (t.t2, (t.t3, (t.t4, (t.t5, (t.t6, (t.t7, (t.t8, (t.t9, (t.t10, (t.t11, (t.t12, (t.t13, (t.t14, (t.t15, (t.t16, (t.t17, (t.t18, (t.t19, (t.t20, (t.t21, (t.t22, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta0Inverse[F[_]]: Inj[F[A1], ProdT[F]] = Inj.instance(_.run.t1)

    implicit def lifta1[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A2]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (x, (t.t3, (t.t4, (t.t5, (t.t6, (t.t7, (t.t8, (t.t9, (t.t10, (t.t11, (t.t12, (t.t13, (t.t14, (t.t15, (t.t16, (t.t17, (t.t18, (t.t19, (t.t20, (t.t21, (t.t22, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta1Inverse[F[_]]: Inj[F[A2], ProdT[F]] = Inj.instance(_.run.t2)

    implicit def lifta2[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A3]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (t.t2, (x, (t.t4, (t.t5, (t.t6, (t.t7, (t.t8, (t.t9, (t.t10, (t.t11, (t.t12, (t.t13, (t.t14, (t.t15, (t.t16, (t.t17, (t.t18, (t.t19, (t.t20, (t.t21, (t.t22, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta2Inverse[F[_]]: Inj[F[A3], ProdT[F]] = Inj.instance(_.run.t3)

    implicit def lifta3[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A4]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (t.t2, (t.t3, (x, (t.t5, (t.t6, (t.t7, (t.t8, (t.t9, (t.t10, (t.t11, (t.t12, (t.t13, (t.t14, (t.t15, (t.t16, (t.t17, (t.t18, (t.t19, (t.t20, (t.t21, (t.t22, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta3Inverse[F[_]]: Inj[F[A4], ProdT[F]] = Inj.instance(_.run.t4)

    implicit def lifta4[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A5]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (t.t2, (t.t3, (t.t4, (x, (t.t6, (t.t7, (t.t8, (t.t9, (t.t10, (t.t11, (t.t12, (t.t13, (t.t14, (t.t15, (t.t16, (t.t17, (t.t18, (t.t19, (t.t20, (t.t21, (t.t22, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta4Inverse[F[_]]: Inj[F[A5], ProdT[F]] = Inj.instance(_.run.t5)

    implicit def lifta5[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A6]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (t.t2, (t.t3, (t.t4, (t.t5, (x, (t.t7, (t.t8, (t.t9, (t.t10, (t.t11, (t.t12, (t.t13, (t.t14, (t.t15, (t.t16, (t.t17, (t.t18, (t.t19, (t.t20, (t.t21, (t.t22, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta5Inverse[F[_]]: Inj[F[A6], ProdT[F]] = Inj.instance(_.run.t6)

    implicit def lifta6[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A7]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (t.t2, (t.t3, (t.t4, (t.t5, (t.t6, (x, (t.t8, (t.t9, (t.t10, (t.t11, (t.t12, (t.t13, (t.t14, (t.t15, (t.t16, (t.t17, (t.t18, (t.t19, (t.t20, (t.t21, (t.t22, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta6Inverse[F[_]]: Inj[F[A7], ProdT[F]] = Inj.instance(_.run.t7)

    implicit def lifta7[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A8]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (t.t2, (t.t3, (t.t4, (t.t5, (t.t6, (t.t7, (x, (t.t9, (t.t10, (t.t11, (t.t12, (t.t13, (t.t14, (t.t15, (t.t16, (t.t17, (t.t18, (t.t19, (t.t20, (t.t21, (t.t22, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta7Inverse[F[_]]: Inj[F[A8], ProdT[F]] = Inj.instance(_.run.t8)

    implicit def lifta8[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A9]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (t.t2, (t.t3, (t.t4, (t.t5, (t.t6, (t.t7, (t.t8, (x, (t.t10, (t.t11, (t.t12, (t.t13, (t.t14, (t.t15, (t.t16, (t.t17, (t.t18, (t.t19, (t.t20, (t.t21, (t.t22, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta8Inverse[F[_]]: Inj[F[A9], ProdT[F]] = Inj.instance(_.run.t9)

    implicit def lifta9[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A10]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (t.t2, (t.t3, (t.t4, (t.t5, (t.t6, (t.t7, (t.t8, (t.t9, (x, (t.t11, (t.t12, (t.t13, (t.t14, (t.t15, (t.t16, (t.t17, (t.t18, (t.t19, (t.t20, (t.t21, (t.t22, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta9Inverse[F[_]]: Inj[F[A10], ProdT[F]] = Inj.instance(_.run.t10)

    implicit def lifta10[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A11]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (t.t2, (t.t3, (t.t4, (t.t5, (t.t6, (t.t7, (t.t8, (t.t9, (t.t10, (x, (t.t12, (t.t13, (t.t14, (t.t15, (t.t16, (t.t17, (t.t18, (t.t19, (t.t20, (t.t21, (t.t22, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta10Inverse[F[_]]: Inj[F[A11], ProdT[F]] = Inj.instance(_.run.t11)

    implicit def lifta11[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A12]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (t.t2, (t.t3, (t.t4, (t.t5, (t.t6, (t.t7, (t.t8, (t.t9, (t.t10, (t.t11, (x, (t.t13, (t.t14, (t.t15, (t.t16, (t.t17, (t.t18, (t.t19, (t.t20, (t.t21, (t.t22, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta11Inverse[F[_]]: Inj[F[A12], ProdT[F]] = Inj.instance(_.run.t12)

    implicit def lifta12[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A13]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (t.t2, (t.t3, (t.t4, (t.t5, (t.t6, (t.t7, (t.t8, (t.t9, (t.t10, (t.t11, (t.t12, (x, (t.t14, (t.t15, (t.t16, (t.t17, (t.t18, (t.t19, (t.t20, (t.t21, (t.t22, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta12Inverse[F[_]]: Inj[F[A13], ProdT[F]] = Inj.instance(_.run.t13)

    implicit def lifta13[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A14]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (t.t2, (t.t3, (t.t4, (t.t5, (t.t6, (t.t7, (t.t8, (t.t9, (t.t10, (t.t11, (t.t12, (t.t13, (x, (t.t15, (t.t16, (t.t17, (t.t18, (t.t19, (t.t20, (t.t21, (t.t22, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta13Inverse[F[_]]: Inj[F[A14], ProdT[F]] = Inj.instance(_.run.t14)

    implicit def lifta14[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A15]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (t.t2, (t.t3, (t.t4, (t.t5, (t.t6, (t.t7, (t.t8, (t.t9, (t.t10, (t.t11, (t.t12, (t.t13, (t.t14, (x, (t.t16, (t.t17, (t.t18, (t.t19, (t.t20, (t.t21, (t.t22, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta14Inverse[F[_]]: Inj[F[A15], ProdT[F]] = Inj.instance(_.run.t15)

    implicit def lifta15[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A16]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (t.t2, (t.t3, (t.t4, (t.t5, (t.t6, (t.t7, (t.t8, (t.t9, (t.t10, (t.t11, (t.t12, (t.t13, (t.t14, (t.t15, (x, (t.t17, (t.t18, (t.t19, (t.t20, (t.t21, (t.t22, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta15Inverse[F[_]]: Inj[F[A16], ProdT[F]] = Inj.instance(_.run.t16)

    implicit def lifta16[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A17]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (t.t2, (t.t3, (t.t4, (t.t5, (t.t6, (t.t7, (t.t8, (t.t9, (t.t10, (t.t11, (t.t12, (t.t13, (t.t14, (t.t15, (t.t16, (x, (t.t18, (t.t19, (t.t20, (t.t21, (t.t22, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta16Inverse[F[_]]: Inj[F[A17], ProdT[F]] = Inj.instance(_.run.t17)

    implicit def lifta17[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A18]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (t.t2, (t.t3, (t.t4, (t.t5, (t.t6, (t.t7, (t.t8, (t.t9, (t.t10, (t.t11, (t.t12, (t.t13, (t.t14, (t.t15, (t.t16, (t.t17, (x, (t.t19, (t.t20, (t.t21, (t.t22, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta17Inverse[F[_]]: Inj[F[A18], ProdT[F]] = Inj.instance(_.run.t18)

    implicit def lifta18[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A19]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (t.t2, (t.t3, (t.t4, (t.t5, (t.t6, (t.t7, (t.t8, (t.t9, (t.t10, (t.t11, (t.t12, (t.t13, (t.t14, (t.t15, (t.t16, (t.t17, (t.t18, (x, (t.t20, (t.t21, (t.t22, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta18Inverse[F[_]]: Inj[F[A19], ProdT[F]] = Inj.instance(_.run.t19)

    implicit def lifta19[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A20]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (t.t2, (t.t3, (t.t4, (t.t5, (t.t6, (t.t7, (t.t8, (t.t9, (t.t10, (t.t11, (t.t12, (t.t13, (t.t14, (t.t15, (t.t16, (t.t17, (t.t18, (t.t19, (x, (t.t21, (t.t22, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta19Inverse[F[_]]: Inj[F[A20], ProdT[F]] = Inj.instance(_.run.t20)

    implicit def lifta20[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A21]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (t.t2, (t.t3, (t.t4, (t.t5, (t.t6, (t.t7, (t.t8, (t.t9, (t.t10, (t.t11, (t.t12, (t.t13, (t.t14, (t.t15, (t.t16, (t.t17, (t.t18, (t.t19, (t.t20, (x, (t.t22, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta20Inverse[F[_]]: Inj[F[A21], ProdT[F]] = Inj.instance(_.run.t21)

    implicit def lifta21[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A22]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (t.t2, (t.t3, (t.t4, (t.t5, (t.t6, (t.t7, (t.t8, (t.t9, (t.t10, (t.t11, (t.t12, (t.t13, (t.t14, (t.t15, (t.t16, (t.t17, (t.t18, (t.t19, (t.t20, (t.t21, (x, t.t23)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta21Inverse[F[_]]: Inj[F[A22], ProdT[F]] = Inj.instance(_.run.t22)

    implicit def lifta22[F[_]](implicit M: Monoid[ProdT[F]]): Inj[ProdT[F], F[A23]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          ProdT(
            (
              t.t1,
              (t.t2, (t.t3, (t.t4, (t.t5, (t.t6, (t.t7, (t.t8, (t.t9, (t.t10, (t.t11, (t.t12, (t.t13, (t.t14, (t.t15, (t.t16, (t.t17, (t.t18, (t.t19, (t.t20, (t.t21, (t.t22, x)))))))))))))))))))))
            )
          )
      )
    }

    implicit def lifta22Inverse[F[_]]: Inj[F[A23], ProdT[F]] = Inj.instance(_.run.t23)

  }

  type Prod[F[_]] = ProdT[F]

  case class CopT[F[_]](
      run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[
        A19
      ] \/ (F[A20] \/ (F[A21] \/ (F[A22] \/ F[A23]))))))))))))))))))))))
  )
  object CopT {

    implicit def prisma0[F[_]]: Prism[CopT[F], F[A1]] = new Prism[CopT[F], F[A1]] {
      def getOption(c: CopT[F]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _      => None
      }
      def reverseGet(x: F[A1]): CopT[F] = CopT(-\/(x))
    }

    implicit def inja0[F[_]]: Inj[CopT[F], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_]]: Inj[Option[F[A1]], CopT[F]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_]]: Prism[CopT[F], F[A2]] = new Prism[CopT[F], F[A2]] {
      def getOption(c: CopT[F]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _           => None
      }
      def reverseGet(x: F[A2]): CopT[F] = CopT(\/-(-\/(x)))
    }

    implicit def inja1[F[_]]: Inj[CopT[F], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_]]: Inj[Option[F[A2]], CopT[F]] = Inj.instance(prisma1.getOption(_))

    implicit def prisma2[F[_]]: Prism[CopT[F], F[A3]] = new Prism[CopT[F], F[A3]] {
      def getOption(c: CopT[F]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _                => None
      }
      def reverseGet(x: F[A3]): CopT[F] = CopT(\/-(\/-(-\/(x))))
    }

    implicit def inja2[F[_]]: Inj[CopT[F], F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit def inja2Inverse[F[_]]: Inj[Option[F[A3]], CopT[F]] = Inj.instance(prisma2.getOption(_))

    implicit def prisma3[F[_]]: Prism[CopT[F], F[A4]] = new Prism[CopT[F], F[A4]] {
      def getOption(c: CopT[F]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _                     => None
      }
      def reverseGet(x: F[A4]): CopT[F] = CopT(\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3[F[_]]: Inj[CopT[F], F[A4]] = Inj.instance(prisma3.reverseGet(_))
    implicit def inja3Inverse[F[_]]: Inj[Option[F[A4]], CopT[F]] = Inj.instance(prisma3.getOption(_))

    implicit def prisma4[F[_]]: Prism[CopT[F], F[A5]] = new Prism[CopT[F], F[A5]] {
      def getOption(c: CopT[F]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _                          => None
      }
      def reverseGet(x: F[A5]): CopT[F] = CopT(\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4[F[_]]: Inj[CopT[F], F[A5]] = Inj.instance(prisma4.reverseGet(_))
    implicit def inja4Inverse[F[_]]: Inj[Option[F[A5]], CopT[F]] = Inj.instance(prisma4.getOption(_))

    implicit def prisma5[F[_]]: Prism[CopT[F], F[A6]] = new Prism[CopT[F], F[A6]] {
      def getOption(c: CopT[F]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _                               => None
      }
      def reverseGet(x: F[A6]): CopT[F] = CopT(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
    }

    implicit def inja5[F[_]]: Inj[CopT[F], F[A6]] = Inj.instance(prisma5.reverseGet(_))
    implicit def inja5Inverse[F[_]]: Inj[Option[F[A6]], CopT[F]] = Inj.instance(prisma5.getOption(_))

    implicit def prisma6[F[_]]: Prism[CopT[F], F[A7]] = new Prism[CopT[F], F[A7]] {
      def getOption(c: CopT[F]): Option[F[A7]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
        case _                                    => None
      }
      def reverseGet(x: F[A7]): CopT[F] = CopT(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
    }

    implicit def inja6[F[_]]: Inj[CopT[F], F[A7]] = Inj.instance(prisma6.reverseGet(_))
    implicit def inja6Inverse[F[_]]: Inj[Option[F[A7]], CopT[F]] = Inj.instance(prisma6.getOption(_))

    implicit def prisma7[F[_]]: Prism[CopT[F], F[A8]] = new Prism[CopT[F], F[A8]] {
      def getOption(c: CopT[F]): Option[F[A8]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
        case _                                         => None
      }
      def reverseGet(x: F[A8]): CopT[F] = CopT(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
    }

    implicit def inja7[F[_]]: Inj[CopT[F], F[A8]] = Inj.instance(prisma7.reverseGet(_))
    implicit def inja7Inverse[F[_]]: Inj[Option[F[A8]], CopT[F]] = Inj.instance(prisma7.getOption(_))

    implicit def prisma8[F[_]]: Prism[CopT[F], F[A9]] = new Prism[CopT[F], F[A9]] {
      def getOption(c: CopT[F]): Option[F[A9]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
        case _                                              => None
      }
      def reverseGet(x: F[A9]): CopT[F] = CopT(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
    }

    implicit def inja8[F[_]]: Inj[CopT[F], F[A9]] = Inj.instance(prisma8.reverseGet(_))
    implicit def inja8Inverse[F[_]]: Inj[Option[F[A9]], CopT[F]] = Inj.instance(prisma8.getOption(_))

    implicit def prisma9[F[_]]: Prism[CopT[F], F[A10]] = new Prism[CopT[F], F[A10]] {
      def getOption(c: CopT[F]): Option[F[A10]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
        case _                                                   => None
      }
      def reverseGet(x: F[A10]): CopT[F] = CopT(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
    }

    implicit def inja9[F[_]]: Inj[CopT[F], F[A10]] = Inj.instance(prisma9.reverseGet(_))
    implicit def inja9Inverse[F[_]]: Inj[Option[F[A10]], CopT[F]] = Inj.instance(prisma9.getOption(_))

    implicit def prisma10[F[_]]: Prism[CopT[F], F[A11]] = new Prism[CopT[F], F[A11]] {
      def getOption(c: CopT[F]): Option[F[A11]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
        case _                                                        => None
      }
      def reverseGet(x: F[A11]): CopT[F] = CopT(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
    }

    implicit def inja10[F[_]]: Inj[CopT[F], F[A11]] = Inj.instance(prisma10.reverseGet(_))
    implicit def inja10Inverse[F[_]]: Inj[Option[F[A11]], CopT[F]] = Inj.instance(prisma10.getOption(_))

    implicit def prisma11[F[_]]: Prism[CopT[F], F[A12]] = new Prism[CopT[F], F[A12]] {
      def getOption(c: CopT[F]): Option[F[A12]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
        case _                                                             => None
      }
      def reverseGet(x: F[A12]): CopT[F] = CopT(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))
    }

    implicit def inja11[F[_]]: Inj[CopT[F], F[A12]] = Inj.instance(prisma11.reverseGet(_))
    implicit def inja11Inverse[F[_]]: Inj[Option[F[A12]], CopT[F]] = Inj.instance(prisma11.getOption(_))

    implicit def prisma12[F[_]]: Prism[CopT[F], F[A13]] = new Prism[CopT[F], F[A13]] {
      def getOption(c: CopT[F]): Option[F[A13]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Some(x)
        case _                                                                  => None
      }
      def reverseGet(x: F[A13]): CopT[F] = CopT(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))
    }

    implicit def inja12[F[_]]: Inj[CopT[F], F[A13]] = Inj.instance(prisma12.reverseGet(_))
    implicit def inja12Inverse[F[_]]: Inj[Option[F[A13]], CopT[F]] = Inj.instance(prisma12.getOption(_))

    implicit def prisma13[F[_]]: Prism[CopT[F], F[A14]] = new Prism[CopT[F], F[A14]] {
      def getOption(c: CopT[F]): Option[F[A14]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => Some(x)
        case _                                                                       => None
      }
      def reverseGet(x: F[A14]): CopT[F] = CopT(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))
    }

    implicit def inja13[F[_]]: Inj[CopT[F], F[A14]] = Inj.instance(prisma13.reverseGet(_))
    implicit def inja13Inverse[F[_]]: Inj[Option[F[A14]], CopT[F]] = Inj.instance(prisma13.getOption(_))

    implicit def prisma14[F[_]]: Prism[CopT[F], F[A15]] = new Prism[CopT[F], F[A15]] {
      def getOption(c: CopT[F]): Option[F[A15]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => Some(x)
        case _                                                                            => None
      }
      def reverseGet(x: F[A15]): CopT[F] = CopT(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))
    }

    implicit def inja14[F[_]]: Inj[CopT[F], F[A15]] = Inj.instance(prisma14.reverseGet(_))
    implicit def inja14Inverse[F[_]]: Inj[Option[F[A15]], CopT[F]] = Inj.instance(prisma14.getOption(_))

    implicit def prisma15[F[_]]: Prism[CopT[F], F[A16]] = new Prism[CopT[F], F[A16]] {
      def getOption(c: CopT[F]): Option[F[A16]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) => Some(x)
        case _                                                                                 => None
      }
      def reverseGet(x: F[A16]): CopT[F] = CopT(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))
    }

    implicit def inja15[F[_]]: Inj[CopT[F], F[A16]] = Inj.instance(prisma15.reverseGet(_))
    implicit def inja15Inverse[F[_]]: Inj[Option[F[A16]], CopT[F]] = Inj.instance(prisma15.getOption(_))

    implicit def prisma16[F[_]]: Prism[CopT[F], F[A17]] = new Prism[CopT[F], F[A17]] {
      def getOption(c: CopT[F]): Option[F[A17]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))) => Some(x)
        case _                                                                                      => None
      }
      def reverseGet(x: F[A17]): CopT[F] = CopT(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))
    }

    implicit def inja16[F[_]]: Inj[CopT[F], F[A17]] = Inj.instance(prisma16.reverseGet(_))
    implicit def inja16Inverse[F[_]]: Inj[Option[F[A17]], CopT[F]] = Inj.instance(prisma16.getOption(_))

    implicit def prisma17[F[_]]: Prism[CopT[F], F[A18]] = new Prism[CopT[F], F[A18]] {
      def getOption(c: CopT[F]): Option[F[A18]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))) => Some(x)
        case _                                                                                           => None
      }
      def reverseGet(x: F[A18]): CopT[F] = CopT(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))
    }

    implicit def inja17[F[_]]: Inj[CopT[F], F[A18]] = Inj.instance(prisma17.reverseGet(_))
    implicit def inja17Inverse[F[_]]: Inj[Option[F[A18]], CopT[F]] = Inj.instance(prisma17.getOption(_))

    implicit def prisma18[F[_]]: Prism[CopT[F], F[A19]] = new Prism[CopT[F], F[A19]] {
      def getOption(c: CopT[F]): Option[F[A19]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))) => Some(x)
        case _                                                                                                => None
      }
      def reverseGet(x: F[A19]): CopT[F] = CopT(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))))
    }

    implicit def inja18[F[_]]: Inj[CopT[F], F[A19]] = Inj.instance(prisma18.reverseGet(_))
    implicit def inja18Inverse[F[_]]: Inj[Option[F[A19]], CopT[F]] = Inj.instance(prisma18.getOption(_))

    implicit def prisma19[F[_]]: Prism[CopT[F], F[A20]] = new Prism[CopT[F], F[A20]] {
      def getOption(c: CopT[F]): Option[F[A20]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))) => Some(x)
        case _                                                                                                     => None
      }
      def reverseGet(x: F[A20]): CopT[F] = CopT(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))))
    }

    implicit def inja19[F[_]]: Inj[CopT[F], F[A20]] = Inj.instance(prisma19.reverseGet(_))
    implicit def inja19Inverse[F[_]]: Inj[Option[F[A20]], CopT[F]] = Inj.instance(prisma19.getOption(_))

    implicit def prisma20[F[_]]: Prism[CopT[F], F[A21]] = new Prism[CopT[F], F[A21]] {
      def getOption(c: CopT[F]): Option[F[A21]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))))) => Some(x)
        case _                                                                                                          => None
      }
      def reverseGet(x: F[A21]): CopT[F] = CopT(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))))))
    }

    implicit def inja20[F[_]]: Inj[CopT[F], F[A21]] = Inj.instance(prisma20.reverseGet(_))
    implicit def inja20Inverse[F[_]]: Inj[Option[F[A21]], CopT[F]] = Inj.instance(prisma20.getOption(_))

    implicit def prisma21[F[_]]: Prism[CopT[F], F[A22]] = new Prism[CopT[F], F[A22]] {
      def getOption(c: CopT[F]): Option[F[A22]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))))) => Some(x)
        case _                                                                                                               => None
      }
      def reverseGet(x: F[A22]): CopT[F] = CopT(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))))))
    }

    implicit def inja21[F[_]]: Inj[CopT[F], F[A22]] = Inj.instance(prisma21.reverseGet(_))
    implicit def inja21Inverse[F[_]]: Inj[Option[F[A22]], CopT[F]] = Inj.instance(prisma21.getOption(_))

    implicit def prisma22[F[_]]: Prism[CopT[F], F[A23]] = new Prism[CopT[F], F[A23]] {
      def getOption(c: CopT[F]): Option[F[A23]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))))))))) => Some(x)
        case _                                                                                                               => None
      }
      def reverseGet(x: F[A23]): CopT[F] = CopT(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))))))))))
    }

    implicit def inja22[F[_]]: Inj[CopT[F], F[A23]] = Inj.instance(prisma22.reverseGet(_))
    implicit def inja22Inverse[F[_]]: Inj[Option[F[A23]], CopT[F]] = Inj.instance(prisma22.getOption(_))

  }

  type Cop[F[_]] = CopT[F]

  def combine[F[_], G[_]](
      implicit a0: G[F[A1]],
      a1: G[F[A2]],
      a2: G[F[A3]],
      a3: G[F[A4]],
      a4: G[F[A5]],
      a5: G[F[A6]],
      a6: G[F[A7]],
      a7: G[F[A8]],
      a8: G[F[A9]],
      a9: G[F[A10]],
      a10: G[F[A11]],
      a11: G[F[A12]],
      a12: G[F[A13]],
      a13: G[F[A14]],
      a14: G[F[A15]],
      a15: G[F[A16]],
      a16: G[F[A17]],
      a17: G[F[A18]],
      a18: G[F[A19]],
      a19: G[F[A20]],
      a20: G[F[A21]],
      a21: G[F[A22]],
      a22: G[F[A23]]
  ): ComposeAndXor[F, G, Cop, Prod] =
    new ComposeAndXor[F, G, Cop, Prod] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[G]): G[B] =
        Combine.choose23(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[G]): G[B] =
        Combine.altly23(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22)(x => f(CopT(x)))

      def mkDivide[B](f: B => Prod[F])(implicit d: Divide[G]): G[B] =
        Combine.divide23(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[G]): G[B] =
        Combine.apply23(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22) {
          case (i0, (i1, (i2, (i3, (i4, (i5, (i6, (i7, (i8, (i9, (i10, (i11, (i12, (i13, (i14, (i15, (i16, (i17, (i18, (i19, (i20, (i21, i22)))))))))))))))))))))) =>
            f(ProdT((i0, (i1, (i2, (i3, (i4, (i5, (i6, (i7, (i8, (i9, (i10, (i11, (i12, (i13, (i14, (i15, (i16, (i17, (i18, (i19, (i20, (i21, i22))))))))))))))))))))))))
        }
    }

  def injEv[F[_]] = combine[F, Inj.Aux[Cop[F]]#Out].choose
  def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = combine[F, Inj.Aux[Prod[F]]#Out].divide

  def transformP[F[_], G[_]](nt: (F ~> G)): AndXor23[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23]#Prod[F] => AndXor23[
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20,
    A21,
    A22,
    A23
  ]#Prod[G] =
    (p: AndXor23[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23]#Prod[F]) => {
      val pr = p.run
      ProdT[G](
        (
          nt(pr.t1),
          (
            nt(pr.t2),
            (
              nt(pr.t3),
              (
                nt(pr.t4),
                (
                  nt(pr.t5),
                  (
                    nt(pr.t6),
                    (
                      nt(pr.t7),
                      (
                        nt(pr.t8),
                        (
                          nt(pr.t9),
                          (
                            nt(pr.t10),
                            (
                              nt(pr.t11),
                              (nt(pr.t12), (nt(pr.t13), (nt(pr.t14), (nt(pr.t15), (nt(pr.t16), (nt(pr.t17), (nt(pr.t18), (nt(pr.t19), (nt(pr.t20), (nt(pr.t21), (nt(pr.t22), nt(pr.t23))))))))))))
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    }

  def transformC[F[_], G[_]](nt: (F ~> G)): AndXor23[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23]#Cop[F] => AndXor23[
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20,
    A21,
    A22,
    A23
  ]#Cop[G] =
    (p: AndXor23[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23]#Cop[F]) =>
      CopT[G](
        p.run.bimap(
          nt(_),
          _.bimap(
            nt(_),
            _.bimap(
              nt(_),
              _.bimap(
                nt(_),
                _.bimap(
                  nt(_),
                  _.bimap(
                    nt(_),
                    _.bimap(
                      nt(_),
                      _.bimap(
                        nt(_),
                        _.bimap(
                          nt(_),
                          _.bimap(
                            nt(_),
                            _.bimap(
                              nt(_),
                              _.bimap(
                                nt(_),
                                _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_)))))))))))
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

  // format: off
  def sequenceP[F[_]](prod: Prod[F])(implicit A: Apply[F]): F[Prod[Id]] = {
    val p = prod.run
    A.map(
    A.ap(p.t23)(
    A.ap(p.t22)(
    A.ap(p.t21)(
    A.ap(p.t20)(
    A.ap(p.t19)(
    A.ap(p.t18)(
    A.ap(p.t17)(
    A.ap(p.t16)(
    A.ap(p.t15)(
    A.ap(p.t14)(
    A.ap(p.t13)(
    A.ap(p.t12)(
    A.ap(p.t11)(
    A.ap(p.t10)(
    A.ap(p.t9)(
    A.ap(p.t8)(
    A.ap(p.t7)(
    A.ap(p.t6)(
    A.ap(p.t5)(
    A.ap(p.t4)(
    A.ap(p.t3)(
    A.ap(p.t2)(
    A.map(p.t1)((i0: A1) => (i1: A2) => (i2: A3) => (i3: A4) => (i4: A5) => (i5: A6) => (i6: A7) => (i7: A8) => (i8: A9) => (i9: A10) => (i10: A11) => (i11: A12) => (i12: A13) => (i13: A14) => (i14: A15) => (i15: A16) => (i16: A17) => (i17: A18) => (i18: A19) => (i19: A20) => (i20: A21) => (i21: A22) => (i22: A23) =>
      (i0, (i1, (i2, (i3, (i4, (i5, (i6, (i7, (i8, (i9, (i10, (i11, (i12, (i13, (i14, (i15, (i16, (i17, (i18, (i19, (i20, (i21, i22))))))))))))))))))))))))))))))))))))))))))))))(ProdT[Id](_))
  }

  def sequenceC[F[_]](cop: Cop[F])(implicit FF: Functor[F]): F[Cop[Id]] =
    cop.run match {
      case -\/(x) => FF.map(FF.map(x)(y => -\/(y)))(CopT[Id](_))
      case \/-(-\/(x)) => FF.map(FF.map(x)(y => \/-(-\/(y))))(CopT[Id](_))
      case \/-(\/-(-\/(x))) => FF.map(FF.map(x)(y => \/-(\/-(-\/(y)))))(CopT[Id](_))
      case \/-(\/-(\/-(-\/(x)))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(-\/(y))))))(CopT[Id](_))
      case \/-(\/-(\/-(\/-(-\/(x))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(-\/(y)))))))(CopT[Id](_))
      case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(-\/(y))))))))(CopT[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))(CopT[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))(CopT[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))(CopT[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))(CopT[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))(CopT[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))(CopT[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))(CopT[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))))(CopT[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))))(CopT[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))))))(CopT[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))))))(CopT[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))))))))(CopT[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))))))))(CopT[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))))))))))(CopT[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))))))))))(CopT[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))))))))))))(CopT[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(y))))))))))))))))))))))))(CopT[Id](_))
    }

  def extractC[F[_], B](c: Cop[F])(implicit inj: Inj[Option[B], Cop[F]]): Option[B] = inj(c)

  def extractP[F[_], B](p: Prod[F])(implicit inj: Inj[B, Prod[F]]): B = inj(p)

  def foldMap[F[_], C](p: Prod[F])(map: Cop[F] => C)(implicit M: Monoid[C]): C = {
    val pr = p.run
    M.append(map(CopT.inja0(pr.t1)), M.append(map(CopT.inja1(pr.t2)), M.append(map(CopT.inja2(pr.t3)), M.append(map(CopT.inja3(pr.t4)), M.append(map(CopT.inja4(pr.t5)), M.append(map(CopT.inja5(pr.t6)), M.append(map(CopT.inja6(pr.t7)), M.append(map(CopT.inja7(pr.t8)), M.append(map(CopT.inja8(pr.t9)), M.append(map(CopT.inja9(pr.t10)), M.append(map(CopT.inja10(pr.t11)), M.append(map(CopT.inja11(pr.t12)), M.append(map(CopT.inja12(pr.t13)), M.append(map(CopT.inja13(pr.t14)), M.append(map(CopT.inja14(pr.t15)), M.append(map(CopT.inja15(pr.t16)), M.append(map(CopT.inja16(pr.t17)), M.append(map(CopT.inja17(pr.t18)), M.append(map(CopT.inja18(pr.t19)), M.append(map(CopT.inja19(pr.t20)), M.append(map(CopT.inja20(pr.t21)), M.append(map(CopT.inja21(pr.t22)), map(CopT.inja22(pr.t23))))))))))))))))))))))))
  }

  def foldMapId[F[_], C](p: Prod[F])(map: Cop[Id] => C)(
      implicit O: Ordering[Cop[Id]], M: Monoid[C], PE: PlusEmpty[F], U: Uncons[F]): C = {
    import scala.collection.mutable.{PriorityQueue => PQ}
    def uncons(p: Prod[F]): (List[Cop[Id]], Prod[F]) = {
      val pr = p.run
      val ht1 = U(pr.t1)
      val ht2 = U(pr.t2)
      val ht3 = U(pr.t3)
      val ht4 = U(pr.t4)
      val ht5 = U(pr.t5)
      val ht6 = U(pr.t6)
      val ht7 = U(pr.t7)
      val ht8 = U(pr.t8)
      val ht9 = U(pr.t9)
      val ht10 = U(pr.t10)
      val ht11 = U(pr.t11)
      val ht12 = U(pr.t12)
      val ht13 = U(pr.t13)
      val ht14 = U(pr.t14)
      val ht15 = U(pr.t15)
      val ht16 = U(pr.t16)
      val ht17 = U(pr.t17)
      val ht18 = U(pr.t18)
      val ht19 = U(pr.t19)
      val ht20 = U(pr.t20)
      val ht21 = U(pr.t21)
      val ht22 = U(pr.t22)
      val ht23 = U(pr.t23)
      (List(ht1._1.map(inj(_: Id[A1])), ht2._1.map(inj(_: Id[A2])), ht3._1.map(inj(_: Id[A3])), ht4._1.map(inj(_: Id[A4])), ht5._1.map(inj(_: Id[A5])), ht6._1.map(inj(_: Id[A6])), ht7._1.map(inj(_: Id[A7])), ht8._1.map(inj(_: Id[A8])), ht9._1.map(inj(_: Id[A9])), ht10._1.map(inj(_: Id[A10])), ht11._1.map(inj(_: Id[A11])), ht12._1.map(inj(_: Id[A12])), ht13._1.map(inj(_: Id[A13])), ht14._1.map(inj(_: Id[A14])), ht15._1.map(inj(_: Id[A15])), ht16._1.map(inj(_: Id[A16])), ht17._1.map(inj(_: Id[A17])), ht18._1.map(inj(_: Id[A18])), ht19._1.map(inj(_: Id[A19])), ht20._1.map(inj(_: Id[A20])), ht21._1.map(inj(_: Id[A21])), ht22._1.map(inj(_: Id[A22])), ht23._1.map(inj(_: Id[A23]))).flatten,
        ProdT[F]((ht1._2, (ht2._2, (ht3._2, (ht4._2, (ht5._2, (ht6._2, (ht7._2, (ht8._2, (ht9._2, (ht10._2, (ht11._2, (ht12._2, (ht13._2, (ht14._2, (ht15._2, (ht16._2, (ht17._2, (ht18._2, (ht19._2, (ht20._2, (ht21._2, (ht22._2, ht23._2))))))))))))))))))))))))
    }
    @scala.annotation.tailrec
    def go(prod: Prod[F], q: PQ[Cop[Id]], out: C): C =
      (prod.run.==((PE.empty[A1], (PE.empty[A2], (PE.empty[A3], (PE.empty[A4], (PE.empty[A5], (PE.empty[A6], (PE.empty[A7], (PE.empty[A8], (PE.empty[A9], (PE.empty[A10], (PE.empty[A11], (PE.empty[A12], (PE.empty[A13], (PE.empty[A14], (PE.empty[A15], (PE.empty[A16], (PE.empty[A17], (PE.empty[A18], (PE.empty[A19], (PE.empty[A20], (PE.empty[A21], (PE.empty[A22], PE.empty[A23])))))))))))))))))))))))) match {
        case true =>
          q.foldLeft(out)((acc, el) => M.append(acc, map(el)))
        case false => q.isEmpty match {
          case true => {
            val (hs, ts) = uncons(prod)
            q ++= hs
            go(ts, q, out)
          }
          case false => q.dequeue.run match {
            case -\/(x) => {
              val pr = prod.run
              val (h, t) = U(pr.t1)
              go(ProdT[F]((t, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, (pr.t15, (pr.t16, (pr.t17, (pr.t18, (pr.t19, (pr.t20, (pr.t21, (pr.t22, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A1])), M.append(out, map(inj(x))))
          }
          case \/-(-\/(x)) => {
              val pr = prod.run
              val (h, t) = U(pr.t2)
              go(ProdT[F]((pr.t1, (t, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, (pr.t15, (pr.t16, (pr.t17, (pr.t18, (pr.t19, (pr.t20, (pr.t21, (pr.t22, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A2])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(-\/(x))) => {
              val pr = prod.run
              val (h, t) = U(pr.t3)
              go(ProdT[F]((pr.t1, (pr.t2, (t, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, (pr.t15, (pr.t16, (pr.t17, (pr.t18, (pr.t19, (pr.t20, (pr.t21, (pr.t22, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A3])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(-\/(x)))) => {
              val pr = prod.run
              val (h, t) = U(pr.t4)
              go(ProdT[F]((pr.t1, (pr.t2, (pr.t3, (t, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, (pr.t15, (pr.t16, (pr.t17, (pr.t18, (pr.t19, (pr.t20, (pr.t21, (pr.t22, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A4])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(-\/(x))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t5)
              go(ProdT[F]((pr.t1, (pr.t2, (pr.t3, (pr.t4, (t, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, (pr.t15, (pr.t16, (pr.t17, (pr.t18, (pr.t19, (pr.t20, (pr.t21, (pr.t22, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A5])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t6)
              go(ProdT[F]((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (t, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, (pr.t15, (pr.t16, (pr.t17, (pr.t18, (pr.t19, (pr.t20, (pr.t21, (pr.t22, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A6])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t7)
              go(ProdT[F]((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (t, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, (pr.t15, (pr.t16, (pr.t17, (pr.t18, (pr.t19, (pr.t20, (pr.t21, (pr.t22, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A7])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t8)
              go(ProdT[F]((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (t, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, (pr.t15, (pr.t16, (pr.t17, (pr.t18, (pr.t19, (pr.t20, (pr.t21, (pr.t22, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A8])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t9)
              go(ProdT[F]((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (t, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, (pr.t15, (pr.t16, (pr.t17, (pr.t18, (pr.t19, (pr.t20, (pr.t21, (pr.t22, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A9])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t10)
              go(ProdT[F]((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (t, (pr.t11, (pr.t12, (pr.t13, (pr.t14, (pr.t15, (pr.t16, (pr.t17, (pr.t18, (pr.t19, (pr.t20, (pr.t21, (pr.t22, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A10])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t11)
              go(ProdT[F]((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (t, (pr.t12, (pr.t13, (pr.t14, (pr.t15, (pr.t16, (pr.t17, (pr.t18, (pr.t19, (pr.t20, (pr.t21, (pr.t22, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A11])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t12)
              go(ProdT[F]((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (t, (pr.t13, (pr.t14, (pr.t15, (pr.t16, (pr.t17, (pr.t18, (pr.t19, (pr.t20, (pr.t21, (pr.t22, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A12])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t13)
              go(ProdT[F]((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (t, (pr.t14, (pr.t15, (pr.t16, (pr.t17, (pr.t18, (pr.t19, (pr.t20, (pr.t21, (pr.t22, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A13])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t14)
              go(ProdT[F]((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (t, (pr.t15, (pr.t16, (pr.t17, (pr.t18, (pr.t19, (pr.t20, (pr.t21, (pr.t22, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A14])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t15)
              go(ProdT[F]((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, (t, (pr.t16, (pr.t17, (pr.t18, (pr.t19, (pr.t20, (pr.t21, (pr.t22, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A15])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t16)
              go(ProdT[F]((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, (pr.t15, (t, (pr.t17, (pr.t18, (pr.t19, (pr.t20, (pr.t21, (pr.t22, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A16])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t17)
              go(ProdT[F]((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, (pr.t15, (pr.t16, (t, (pr.t18, (pr.t19, (pr.t20, (pr.t21, (pr.t22, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A17])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t18)
              go(ProdT[F]((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, (pr.t15, (pr.t16, (pr.t17, (t, (pr.t19, (pr.t20, (pr.t21, (pr.t22, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A18])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t19)
              go(ProdT[F]((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, (pr.t15, (pr.t16, (pr.t17, (pr.t18, (t, (pr.t20, (pr.t21, (pr.t22, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A19])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t20)
              go(ProdT[F]((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, (pr.t15, (pr.t16, (pr.t17, (pr.t18, (pr.t19, (t, (pr.t21, (pr.t22, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A20])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t21)
              go(ProdT[F]((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, (pr.t15, (pr.t16, (pr.t17, (pr.t18, (pr.t19, (pr.t20, (t, (pr.t22, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A21])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t22)
              go(ProdT[F]((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, (pr.t15, (pr.t16, (pr.t17, (pr.t18, (pr.t19, (pr.t20, (pr.t21, (t, pr.t23))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A22])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t23)
              go(ProdT[F]((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, (pr.t15, (pr.t16, (pr.t17, (pr.t18, (pr.t19, (pr.t20, (pr.t21, (pr.t22, t))))))))))))))))))))))),
                q ++= h.map(inj(_: Id[A23])), M.append(out, map(inj(x))))
          }

          }
        }
      }
    val Q = new PQ[Cop[Id]]()(O)
    val (hs, ts) = uncons(p)
    Q ++= hs
    go(ts, Q, M.zero)
  }
  // format: on
}

object AndXor23 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23]
    : AndXor23[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23] =
    new AndXor23[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23] {}
}
