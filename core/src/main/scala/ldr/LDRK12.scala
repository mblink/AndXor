package ldr
import scala.language.higherKinds
import scalaz.{Apply, Monoid, \/, ~>}
import scalaz.Id.Id
import scalaz.syntax.either._

trait LDRK12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] extends LDR {
  type Prod = (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12])
  type Cop = (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ F[A12])))))))))))
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
      a11: G[F[A12]]
  ): ComposeLDR[G, Cop, Prod] =
    new ComposeLDR[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose12(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly12(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide12(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply12(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11) => f((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11)))
    }

  import LDRK12._

  val injEv = combine[Inj.Aux[Cop]#Out].choose
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide

  def transformP[G[_]](nt: (F ~> G)): LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod => LDRK12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod =
    (p: LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod) =>
      (nt(p._1), nt(p._2), nt(p._3), nt(p._4), nt(p._5), nt(p._6), nt(p._7), nt(p._8), nt(p._9), nt(p._10), nt(p._11), nt(p._12))

  def transformC[G[_]](nt: (F ~> G)): LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Cop => LDRK12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Cop =
    (p: LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Cop) =>
      p.bimap(
        nt(_),
        _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_)))))))))))
      )
}

object LDRK12 {

  def apply[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] =
    new LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}

  implicit def inja0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Cop, F[A1]] =
    Inj.instance(_.left[(F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ F[A12]))))))))))])

  implicit def inja1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Cop, F[A2]] =
    Inj.instance(_.left[(F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ F[A12])))))))))].right[F[A1]])

  implicit def inja2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Cop, F[A3]] =
    Inj.instance(_.left[(F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ F[A12]))))))))].right[F[A2]].right[F[A1]])

  implicit def inja3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Cop, F[A4]] =
    Inj.instance(_.left[(F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ F[A12])))))))].right[F[A3]].right[F[A2]].right[F[A1]])

  implicit def inja4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Cop, F[A5]] =
    Inj.instance(_.left[(F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ F[A12]))))))].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

  implicit def inja5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Cop, F[A6]] =
    Inj.instance(_.left[(F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ F[A12])))))].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

  implicit def inja6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Cop, F[A7]] =
    Inj.instance(_.left[(F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ F[A12]))))].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

  implicit def inja7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Cop, F[A8]] =
    Inj.instance(_.left[(F[A9] \/ (F[A10] \/ (F[A11] \/ F[A12])))].right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

  implicit def inja8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Cop, F[A9]] =
    Inj.instance(_.left[(F[A10] \/ (F[A11] \/ F[A12]))].right[F[A8]].right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

  implicit def inja9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Cop, F[A10]] =
    Inj.instance(_.left[(F[A11] \/ F[A12])].right[F[A9]].right[F[A8]].right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

  implicit def inja10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Cop, F[A11]] =
    Inj.instance(_.left[F[A12]].right[F[A10]].right[F[A9]].right[F[A8]].right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

  implicit def inja11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Cop, F[A12]] =
    Inj.instance(_.right[F[A11]].right[F[A10]].right[F[A9]].right[F[A8]].right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

  implicit def lifta0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
      implicit M: Monoid[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod]
  ): Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod, F[A1]] = {
    val (_, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) =
      M.zero
    Inj.instance((_, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10))
  }

  implicit def lifta1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
      implicit M: Monoid[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod]
  ): Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod, F[A2]] = {
    val (a0, _, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) =
      M.zero
    Inj.instance((a0, _, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10))
  }

  implicit def lifta2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
      implicit M: Monoid[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod]
  ): Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod, F[A3]] = {
    val (a0, a1, _, a2, a3, a4, a5, a6, a7, a8, a9, a10) =
      M.zero
    Inj.instance((a0, a1, _, a2, a3, a4, a5, a6, a7, a8, a9, a10))
  }

  implicit def lifta3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
      implicit M: Monoid[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod]
  ): Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod, F[A4]] = {
    val (a0, a1, a2, _, a3, a4, a5, a6, a7, a8, a9, a10) =
      M.zero
    Inj.instance((a0, a1, a2, _, a3, a4, a5, a6, a7, a8, a9, a10))
  }

  implicit def lifta4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
      implicit M: Monoid[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod]
  ): Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod, F[A5]] = {
    val (a0, a1, a2, a3, _, a4, a5, a6, a7, a8, a9, a10) =
      M.zero
    Inj.instance((a0, a1, a2, a3, _, a4, a5, a6, a7, a8, a9, a10))
  }

  implicit def lifta5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
      implicit M: Monoid[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod]
  ): Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod, F[A6]] = {
    val (a0, a1, a2, a3, a4, _, a5, a6, a7, a8, a9, a10) =
      M.zero
    Inj.instance((a0, a1, a2, a3, a4, _, a5, a6, a7, a8, a9, a10))
  }

  implicit def lifta6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
      implicit M: Monoid[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod]
  ): Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod, F[A7]] = {
    val (a0, a1, a2, a3, a4, a5, _, a6, a7, a8, a9, a10) =
      M.zero
    Inj.instance((a0, a1, a2, a3, a4, a5, _, a6, a7, a8, a9, a10))
  }

  implicit def lifta7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
      implicit M: Monoid[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod]
  ): Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod, F[A8]] = {
    val (a0, a1, a2, a3, a4, a5, a6, _, a7, a8, a9, a10) =
      M.zero
    Inj.instance((a0, a1, a2, a3, a4, a5, a6, _, a7, a8, a9, a10))
  }

  implicit def lifta8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
      implicit M: Monoid[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod]
  ): Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod, F[A9]] = {
    val (a0, a1, a2, a3, a4, a5, a6, a7, _, a8, a9, a10) =
      M.zero
    Inj.instance((a0, a1, a2, a3, a4, a5, a6, a7, _, a8, a9, a10))
  }

  implicit def lifta9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
      implicit M: Monoid[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod]
  ): Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod, F[A10]] = {
    val (a0, a1, a2, a3, a4, a5, a6, a7, a8, _, a9, a10) =
      M.zero
    Inj.instance((a0, a1, a2, a3, a4, a5, a6, a7, a8, _, a9, a10))
  }

  implicit def lifta10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
      implicit M: Monoid[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod]
  ): Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod, F[A11]] = {
    val (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a10) =
      M.zero
    Inj.instance((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a10))
  }

  implicit def lifta11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
      implicit M: Monoid[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod]
  ): Inj[LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod, F[A12]] = {
    val (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _) =
      M.zero
    Inj.instance((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _))
  }

}

trait LDRF12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {
  type Repr[F[_]] = LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]
  def apply[F[_]]: Repr[F] =
    new LDRK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}
}

object LDRF12 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: LDRF12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] =
    new LDRF12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}
}

trait LDR12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] extends LDRK12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

object LDR12 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: LDR12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] =
    new LDR12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}
}
