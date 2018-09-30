package ldr
import scala.language.higherKinds
import scalaz.{Apply, Monoid, \/}
import scalaz.Id.Id
import scalaz.syntax.either._

trait LDRK8[F[_], A1, A2, A3, A4, A5, A6, A7, A8] extends LDR {
  type Prod = (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8])
  type Cop = (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ F[A8])))))))
  def combine[G[_]](implicit a0: G[F[A1]], a1: G[F[A2]], a2: G[F[A3]], a3: G[F[A4]], a4: G[F[A5]], a5: G[F[A6]], a6: G[F[A7]], a7: G[F[A8]]): ComposeLDR[G, Cop, Prod] =
    new ComposeLDR[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose8(a0, a1, a2, a3, a4, a5, a6, a7)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly8(a0, a1, a2, a3, a4, a5, a6, a7)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide8(a0, a1, a2, a3, a4, a5, a6, a7)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply8(a0, a1, a2, a3, a4, a5, a6, a7)((i0, i1, i2, i3, i4, i5, i6, i7) => f((i0, i1, i2, i3, i4, i5, i6, i7)))
    }

  object instances {

    implicit val inja0: Inj[Cop, F[A1]] =
      Inj.instance(_.left[(F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ F[A8]))))))])

    implicit val inja1: Inj[Cop, F[A2]] =
      Inj.instance(_.left[(F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ F[A8])))))].right[F[A1]])

    implicit val inja2: Inj[Cop, F[A3]] =
      Inj.instance(_.left[(F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ F[A8]))))].right[F[A2]].right[F[A1]])

    implicit val inja3: Inj[Cop, F[A4]] =
      Inj.instance(_.left[(F[A5] \/ (F[A6] \/ (F[A7] \/ F[A8])))].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja4: Inj[Cop, F[A5]] =
      Inj.instance(_.left[(F[A6] \/ (F[A7] \/ F[A8]))].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja5: Inj[Cop, F[A6]] =
      Inj.instance(_.left[(F[A7] \/ F[A8])].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja6: Inj[Cop, F[A7]] =
      Inj.instance(_.left[F[A8]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja7: Inj[Cop, F[A8]] =
      Inj.instance(_.right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit def lifta0(implicit M: Monoid[Prod]): Inj[Prod, F[A1]] = {
      val (_, a0, a1, a2, a3, a4, a5, a6) =
        M.zero
      Inj.instance((_, a0, a1, a2, a3, a4, a5, a6))
    }

    implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, F[A2]] = {
      val (a0, _, a1, a2, a3, a4, a5, a6) =
        M.zero
      Inj.instance((a0, _, a1, a2, a3, a4, a5, a6))
    }

    implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, F[A3]] = {
      val (a0, a1, _, a2, a3, a4, a5, a6) =
        M.zero
      Inj.instance((a0, a1, _, a2, a3, a4, a5, a6))
    }

    implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, F[A4]] = {
      val (a0, a1, a2, _, a3, a4, a5, a6) =
        M.zero
      Inj.instance((a0, a1, a2, _, a3, a4, a5, a6))
    }

    implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, F[A5]] = {
      val (a0, a1, a2, a3, _, a4, a5, a6) =
        M.zero
      Inj.instance((a0, a1, a2, a3, _, a4, a5, a6))
    }

    implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, F[A6]] = {
      val (a0, a1, a2, a3, a4, _, a5, a6) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, _, a5, a6))
    }

    implicit def lifta6(implicit M: Monoid[Prod]): Inj[Prod, F[A7]] = {
      val (a0, a1, a2, a3, a4, a5, _, a6) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, a5, _, a6))
    }

    implicit def lifta7(implicit M: Monoid[Prod]): Inj[Prod, F[A8]] = {
      val (a0, a1, a2, a3, a4, a5, a6, _) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, a5, a6, _))
    }

  }

  import instances._

  val injEv = combine[Inj.Aux[Cop]#Out].choose
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide

}

object LDRK8 {

  def apply[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: LDRK8[F, A1, A2, A3, A4, A5, A6, A7, A8] =
    new LDRK8[F, A1, A2, A3, A4, A5, A6, A7, A8] {}
}

trait LDRF8[A1, A2, A3, A4, A5, A6, A7, A8] {
  type Repr[F[_]] = LDRK8[F, A1, A2, A3, A4, A5, A6, A7, A8]
  def apply[F[_]]: Repr[F] =
    new LDRK8[F, A1, A2, A3, A4, A5, A6, A7, A8] {}
}

object LDRF8 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8]: LDRF8[A1, A2, A3, A4, A5, A6, A7, A8] =
    new LDRF8[A1, A2, A3, A4, A5, A6, A7, A8] {}
}

trait LDR8[A1, A2, A3, A4, A5, A6, A7, A8] extends LDRK8[Id, A1, A2, A3, A4, A5, A6, A7, A8]

object LDR8 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8]: LDR8[A1, A2, A3, A4, A5, A6, A7, A8] =
    new LDR8[A1, A2, A3, A4, A5, A6, A7, A8] {}
}
