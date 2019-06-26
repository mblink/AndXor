package andxor

import scalaz.Apply

sealed trait Deriving[T[_[_]], F[_], TC[_], Co[_[_]], Contra[_[_]]] {
  def mkCovariant[A](f: T[F] => A)(implicit F: Co[TC]): TC[A]
  def mkContravariant[A](f: A => T[F])(implicit F: Contra[TC]): TC[A]

  def covariant(implicit F: Co[TC]): TC[T[F]] = mkCovariant(identity _)
  def contravariant(implicit F: Contra[TC]): TC[T[F]] = mkContravariant(identity _)
}

trait DerivingProd[Prod[_[_]], F[_], TC[_]] extends Deriving[Prod, F, TC, Apply, Divide] {
  def mkApply[A](f: Prod[F] => A)(implicit A: Apply[TC]): TC[A] = mkCovariant(f)
  def mkDivide[A](f: A => Prod[F])(implicit D: Divide[TC]): TC[A] = mkContravariant(f)

  def apply(implicit A: Apply[TC]): TC[Prod[F]] = mkApply(identity _)
  def divide(implicit D: Divide[TC]): TC[Prod[F]] = mkDivide(identity _)
}

trait DerivingCop[Cop[_[_]], F[_], TC[_]] extends Deriving[Cop, F, TC, Alt, Decidable] {
  def mkAlt[A](f: Cop[F] => A)(implicit A: Alt[TC]): TC[A] = mkCovariant(f)
  def mkChoose[A](f: A => Cop[F])(implicit D: Decidable[TC]): TC[A] = mkContravariant(f)

  def alt(implicit A: Alt[TC]): TC[Cop[F]] = mkAlt(identity _)
  def choose(implicit D: Decidable[TC]): TC[Cop[F]] = mkChoose(identity _)
}
