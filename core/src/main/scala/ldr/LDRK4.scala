package ldr
import scala.language.higherKinds
import scalaz.{Apply, Monoid, \/}
import scalaz.Id.Id
import scalaz.syntax.either._

trait LDRK4[F[_], A1, A2, A3, A4] extends LDR {
  type Prod = (F[A1], F[A2], F[A3], F[A4])
  type Cop = (F[A1] \/ (F[A2] \/ (F[A3] \/ F[A4])))
  def combine[G[_]](implicit a0: G[F[A1]], a1: G[F[A2]], a2: G[F[A3]], a3: G[F[A4]]): ComposeLDR[G, Cop, Prod] =
    new ComposeLDR[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose4(a0, a1, a2, a3)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly4(a0, a1, a2, a3)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide4(a0, a1, a2, a3)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply4(a0, a1, a2, a3)((i0, i1, i2, i3) => f((i0, i1, i2, i3)))
    }

  object instances {

    implicit val inja0: Inj[Cop, F[A1]] =
      Inj.instance(_.left[(F[A2] \/ (F[A3] \/ F[A4]))])

    implicit val inja1: Inj[Cop, F[A2]] =
      Inj.instance(_.left[(F[A3] \/ F[A4])].right[F[A1]])

    implicit val inja2: Inj[Cop, F[A3]] =
      Inj.instance(_.left[F[A4]].right[F[A2]].right[F[A1]])

    implicit val inja3: Inj[Cop, F[A4]] =
      Inj.instance(_.right[F[A3]].right[F[A2]].right[F[A1]])

    implicit def lifta0(implicit M: Monoid[Prod]): Inj[Prod, F[A1]] = {
      val (_, a0, a1, a2) =
        M.zero
      Inj.instance((_, a0, a1, a2))
    }

    implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, F[A2]] = {
      val (a0, _, a1, a2) =
        M.zero
      Inj.instance((a0, _, a1, a2))
    }

    implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, F[A3]] = {
      val (a0, a1, _, a2) =
        M.zero
      Inj.instance((a0, a1, _, a2))
    }

    implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, F[A4]] = {
      val (a0, a1, a2, _) =
        M.zero
      Inj.instance((a0, a1, a2, _))
    }

  }

  import instances._

  val injEv = combine[Inj.Aux[Cop]#Out].choose
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide

}

object LDRK4 {

  def apply[F[_], A1, A2, A3, A4]: LDRK4[F, A1, A2, A3, A4] =
    new LDRK4[F, A1, A2, A3, A4] {}
}

trait LDRF4[A1, A2, A3, A4] {
  type Repr[F[_]] = LDRK4[F, A1, A2, A3, A4]
  def apply[F[_]]: Repr[F] =
    new LDRK4[F, A1, A2, A3, A4] {}
}

object LDRF4 {
  def apply[A1, A2, A3, A4]: LDRF4[A1, A2, A3, A4] =
    new LDRF4[A1, A2, A3, A4] {}
}

trait LDR4[A1, A2, A3, A4] extends LDRK4[Id, A1, A2, A3, A4]

object LDR4 {
  def apply[A1, A2, A3, A4]: LDR4[A1, A2, A3, A4] =
    new LDR4[A1, A2, A3, A4] {}
}
