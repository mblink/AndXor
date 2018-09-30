package ldr

import scala.language.higherKinds
import scalaz.{Apply, Monoid}

abstract class ComposeLDR[F[_], Cop, Prod] {
  def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B]
  def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B]
  def mkDivide[B](f: B => Prod)(implicit a: Divide[F]): F[B]
  def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B]

  def choose(implicit d: Decidable[F]): F[Cop] = mkChoose(identity _)

  def alt(implicit a: Alt[F]): F[Cop] = mkAlt(identity _)

  def divide(implicit d: Divide[F]): F[Prod] = mkDivide(identity _)

  def apply(implicit a: Apply[F]): F[Prod] = mkApply(identity _)
}

trait LDR {
  type Cop
  type Prod
  val injEv: Inj[Cop, Cop]
  def inj[A](a: A)(implicit inj: Inj[Cop, A]): Cop = inj(a)
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod]
  def lift[A](a: A)(implicit inj: Inj[Prod, A]): Prod = inj(a)
}
