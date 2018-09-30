package ldr
import scala.language.higherKinds
import scalaz.{Apply, Monoid, \/}
import scalaz.Id.Id
import scalaz.syntax.either._

trait LDRK14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] extends LDR {
  type Prod = (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14])
  type Cop = (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14])))))))))))))
  def combine[G[_]](
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
      a13: G[F[A14]]
  ): ComposeLDR[G, Cop, Prod] =
    new ComposeLDR[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose14(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly14(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide14(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply14(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(
          (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13) => f((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13))
        )
    }

  object instances {

    implicit val inja0: Inj[Cop, F[A1]] =
      Inj.instance(_.left[(F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14]))))))))))))])

    implicit val inja1: Inj[Cop, F[A2]] =
      Inj.instance(_.left[(F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14])))))))))))].right[F[A1]])

    implicit val inja2: Inj[Cop, F[A3]] =
      Inj.instance(_.left[(F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14]))))))))))].right[F[A2]].right[F[A1]])

    implicit val inja3: Inj[Cop, F[A4]] =
      Inj.instance(_.left[(F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14])))))))))].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja4: Inj[Cop, F[A5]] =
      Inj.instance(_.left[(F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14]))))))))].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja5: Inj[Cop, F[A6]] =
      Inj.instance(_.left[(F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14])))))))].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja6: Inj[Cop, F[A7]] =
      Inj.instance(_.left[(F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14]))))))].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja7: Inj[Cop, F[A8]] =
      Inj.instance(_.left[(F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14])))))].right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja8: Inj[Cop, F[A9]] =
      Inj.instance(_.left[(F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14]))))].right[F[A8]].right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja9: Inj[Cop, F[A10]] =
      Inj.instance(_.left[(F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14])))].right[F[A9]].right[F[A8]].right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja10: Inj[Cop, F[A11]] =
      Inj.instance(_.left[(F[A12] \/ (F[A13] \/ F[A14]))].right[F[A10]].right[F[A9]].right[F[A8]].right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja11: Inj[Cop, F[A12]] =
      Inj.instance(_.left[(F[A13] \/ F[A14])].right[F[A11]].right[F[A10]].right[F[A9]].right[F[A8]].right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja12: Inj[Cop, F[A13]] =
      Inj.instance(_.left[F[A14]].right[F[A12]].right[F[A11]].right[F[A10]].right[F[A9]].right[F[A8]].right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja13: Inj[Cop, F[A14]] =
      Inj.instance(_.right[F[A13]].right[F[A12]].right[F[A11]].right[F[A10]].right[F[A9]].right[F[A8]].right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit def lifta0(implicit M: Monoid[Prod]): Inj[Prod, F[A1]] = {
      val (_, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) =
        M.zero
      Inj.instance((_, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12))
    }

    implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, F[A2]] = {
      val (a0, _, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) =
        M.zero
      Inj.instance((a0, _, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12))
    }

    implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, F[A3]] = {
      val (a0, a1, _, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) =
        M.zero
      Inj.instance((a0, a1, _, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12))
    }

    implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, F[A4]] = {
      val (a0, a1, a2, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) =
        M.zero
      Inj.instance((a0, a1, a2, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12))
    }

    implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, F[A5]] = {
      val (a0, a1, a2, a3, _, a4, a5, a6, a7, a8, a9, a10, a11, a12) =
        M.zero
      Inj.instance((a0, a1, a2, a3, _, a4, a5, a6, a7, a8, a9, a10, a11, a12))
    }

    implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, F[A6]] = {
      val (a0, a1, a2, a3, a4, _, a5, a6, a7, a8, a9, a10, a11, a12) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, _, a5, a6, a7, a8, a9, a10, a11, a12))
    }

    implicit def lifta6(implicit M: Monoid[Prod]): Inj[Prod, F[A7]] = {
      val (a0, a1, a2, a3, a4, a5, _, a6, a7, a8, a9, a10, a11, a12) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, a5, _, a6, a7, a8, a9, a10, a11, a12))
    }

    implicit def lifta7(implicit M: Monoid[Prod]): Inj[Prod, F[A8]] = {
      val (a0, a1, a2, a3, a4, a5, a6, _, a7, a8, a9, a10, a11, a12) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, a5, a6, _, a7, a8, a9, a10, a11, a12))
    }

    implicit def lifta8(implicit M: Monoid[Prod]): Inj[Prod, F[A9]] = {
      val (a0, a1, a2, a3, a4, a5, a6, a7, _, a8, a9, a10, a11, a12) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, a5, a6, a7, _, a8, a9, a10, a11, a12))
    }

    implicit def lifta9(implicit M: Monoid[Prod]): Inj[Prod, F[A10]] = {
      val (a0, a1, a2, a3, a4, a5, a6, a7, a8, _, a9, a10, a11, a12) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, a5, a6, a7, a8, _, a9, a10, a11, a12))
    }

    implicit def lifta10(implicit M: Monoid[Prod]): Inj[Prod, F[A11]] = {
      val (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a10, a11, a12) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a10, a11, a12))
    }

    implicit def lifta11(implicit M: Monoid[Prod]): Inj[Prod, F[A12]] = {
      val (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a11, a12) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a11, a12))
    }

    implicit def lifta12(implicit M: Monoid[Prod]): Inj[Prod, F[A13]] = {
      val (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a12) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a12))
    }

    implicit def lifta13(implicit M: Monoid[Prod]): Inj[Prod, F[A14]] = {
      val (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _))
    }

  }

  import instances._

  val injEv = combine[Inj.Aux[Cop]#Out].choose
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide

}

object LDRK14 {

  def apply[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: LDRK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
    new LDRK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {}
}

trait LDRF14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {
  type Repr[F[_]] = LDRK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]
  def apply[F[_]]: Repr[F] =
    new LDRK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {}
}

object LDRF14 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: LDRF14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
    new LDRF14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {}
}

trait LDR14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] extends LDRK14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

object LDR14 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: LDR14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
    new LDR14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {}
}
