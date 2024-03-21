package andxor

import cats.Apply

sealed trait Derivation[Cop[_[_]], Prod[_[_]] <: Tuple] { self =>
  protected val a: AndXorNonEmpty {
    type Cop[f[_]] = self.Cop[f]
    type Prod[f[_]] = self.Prod[f]
  }

  // Covariant coproduct
  def deriveCovariant[TC[_], F[_], A](iso: Iso[A, Cop[F]])(
    using F: Alt[TC],
    I: AndXorInstances[TC, Prod[F]],
  ): TC[A]

  // Covariant product
  def deriveCovariant[TC[_], F[_], A](iso: Iso[A, Prod[F]])(
    using F: Apply[TC],
    I: AndXorInstances[TC, Prod[F]],
  ): TC[A]

  // Contravariant coproduct
  def deriveContravariant[TC[_], F[_], A](iso: Iso[A, Cop[F]])(
    using F: Decidable[TC],
    I: AndXorInstances[TC, Prod[F]],
  ): TC[A]

  // Contravariant product
  def deriveContravariant[TC[_], F[_], A](iso: Iso[A, Prod[F]])(
    using F: Divide[TC],
    I: AndXorInstances[TC, Prod[F]],
  ): TC[A]
}

object Derivation {
  given andxor1[X]: Derivation[[F[_]] =>> F[X], [F[_]] =>> F[X] *: EmptyTuple] =
    new Derivation[[F[_]] =>> F[X], [F[_]] =>> F[X] *: EmptyTuple] {
      protected val a: AndXor1[X] = axo[X]

      def deriveCovariant[TC[_], F[_], A](iso: Iso[A, a.Cop[F]])(using F: Alt[TC], I: AndXorInstances[TC, a.Prod[F]]): TC[A] =
        F.map(a.deriving[TC, F].alt)(iso.reverseGet)

      def deriveCovariant[TC[_], F[_], A](iso: Iso[A, a.Prod[F]])(using F: Apply[TC], I: AndXorInstances[TC, a.Prod[F]]): TC[A] =
        F.map(a.deriving[TC, F].apply)(iso.reverseGet)

      def deriveContravariant[TC[_], F[_], A](iso: Iso[A, a.Cop[F]])(using F: Decidable[TC], I: AndXorInstances[TC, a.Prod[F]]): TC[A] =
        F.contramap(a.deriving[TC, F].choose)(iso.get)

      def deriveContravariant[TC[_], F[_], A](iso: Iso[A, a.Prod[F]])(using F: Divide[TC], I: AndXorInstances[TC, a.Prod[F]]): TC[A] =
        F.contramap(a.deriving[TC, F].divide)(iso.get)
    }

  given andxorN[H, CT[_[_]], PT[_[_]] <: Tuple](
    using dt: Derivation[CT, PT]
  ): Derivation[[F[_]] =>> F[H] |: CT[F], [F[_]] =>> F[H] *: PT[F]] =
    new Derivation[[F[_]] =>> F[H] |: CT[F], [F[_]] =>> F[H] *: PT[F]] {
      protected val a: AndXorNext[H, dt.a.type] = (axo[H] *: dt.a).asInstanceOf[AndXorNext[H, dt.a.type]]

      def deriveCovariant[TC[_], F[_], A](iso: Iso[A, a.Cop[F]])(using F: Alt[TC], I: AndXorInstances[TC, a.Prod[F]]): TC[A] =
        F.map(a.deriving[TC, F].alt)(iso.reverseGet)

      def deriveCovariant[TC[_], F[_], A](iso: Iso[A, a.Prod[F]])(using F: Apply[TC], I: AndXorInstances[TC, a.Prod[F]]): TC[A] =
        F.map(a.deriving[TC, F].apply)(iso.reverseGet)

      def deriveContravariant[TC[_], F[_], A](iso: Iso[A, a.Cop[F]])(using F: Decidable[TC], I: AndXorInstances[TC, a.Prod[F]]): TC[A] =
        F.contramap(a.deriving[TC, F].choose)(iso.get)

      def deriveContravariant[TC[_], F[_], A](iso: Iso[A, a.Prod[F]])(using F: Divide[TC], I: AndXorInstances[TC, a.Prod[F]]): TC[A] =
        F.contramap(a.deriving[TC, F].divide)(iso.get)
    }
}
