package andxor

import scalaz.{Apply, InvariantFunctor}

sealed trait DerivingIso[T[_[_]], F[_], TC[_]] {
  def derive[A](to: A => T[F], from: T[F] => A)(implicit F: InvariantFunctor[TC], TC: TC[A]): TC[T[F]] =
    F.xmap(TC, to, from)
}

sealed trait Deriving[T[_[_]], F[_], TC[_], Co[_[_]], Contra[_[_]]] {
  def mkCovariant[A](f: T[F] => A)(implicit F: Co[TC]): TC[A]
  def mkContravariant[A](f: A => T[F])(implicit F: Contra[TC]): TC[A]

  def covariant(implicit F: Co[TC]): TC[T[F]] = mkCovariant(identity _)
  def contravariant(implicit F: Contra[TC]): TC[T[F]] = mkContravariant(identity _)
}

trait DerivingProd[Prod[_[_]], F[_], TC[_]] extends Deriving[Prod, F, TC, Apply, Divide] {
  def apply(implicit A: Apply[TC]): TC[Prod[F]] = mkCovariant(identity _)
  def divide(implicit D: Divide[TC]): TC[Prod[F]] = mkContravariant(identity _)
}

trait DerivingCop[Cop[_[_]], F[_], TC[_]] extends Deriving[Cop, F, TC, Alt, Decidable] {
  def alt(implicit A: Alt[TC]): TC[Cop[F]] = mkCovariant(identity _)
  def choose(implicit D: Decidable[TC]): TC[Cop[F]] = mkContravariant(identity _)
}
