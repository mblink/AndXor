package andxor

import scalaz.Apply
import scalaz.Id.Id
import scalaz.Isomorphism.<=>

object derivation {

  trait Derivation1P[F[_], A1] {
    val axo = AndXor1[A1]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.map(axo.apply[TC, F])(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.contramap(axo.divide[TC, F])(iso.to(_))
  }

  trait Derivation2P[F[_], A1 <: AndXor, A2 <: AndXor] {
    val axo = AndXor2[A1, A2]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.map(axo.apply[TC, F])(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.contramap(axo.divide[TC, F])(iso.to(_))
  }

  trait LPSyntax {

    implicit class Derivation1POps[A, F[_], A1](iso: A <=> AndXor1[A1]#Prod[F]) {
      val derive = new Derivation1P[F, A1] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation2POps[A, F[_], A1 <: AndXor, A2 <: AndXor](iso: A <=> AndXor2[A1, A2]#Prod[F]) {
      val derive = new Derivation2P[F, A1, A2] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

  }

  object syntax extends LPSyntax {

    implicit class Derivation1POps[A, A1](iso: A <=> AndXor1[A1]#Prod[Id]) {
      val derive = new Derivation1P[Id, A1] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation2POps[A, A1 <: AndXor, A2 <: AndXor](iso: A <=> AndXor2[A1, A2]#Prod[Id]) {
      val derive = new Derivation2P[Id, A1, A2] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

  }
}
