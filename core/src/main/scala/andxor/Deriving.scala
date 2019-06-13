package andxor

import scalaz.Apply

trait DerivingProd[Prod[_[_]], F[_], TC[_]] {
  def mkDivide[A](f: A => Prod[F])(implicit D: Divide[TC]): TC[A]
  def mkApply[A](f: Prod[F] => A)(implicit A: Apply[TC]): TC[A]

  def divide(implicit D: Divide[TC]): TC[Prod[F]] = mkDivide(identity _)
  def apply(implicit A: Apply[TC]): TC[Prod[F]] = mkApply(identity _)
}

trait DerivingCop[Cop[_[_]], F[_], TC[_]] {
  def mkAlt[A](f: Cop[F] => A)(implicit A: Alt[TC]): TC[A]
  def mkChoose[A](f: A => Cop[F])(implicit D: Decidable[TC]): TC[A]

  def alt(implicit A: Alt[TC]): TC[Cop[F]] = mkAlt(identity _)
  def choose(implicit D: Decidable[TC]): TC[Cop[F]] = mkChoose(identity _)
}
