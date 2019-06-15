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

  trait Derivation3P[F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor] {
    val axo = AndXor3[A1, A2, A3]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.map(axo.apply[TC, F])(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.contramap(axo.divide[TC, F])(iso.to(_))
  }

  trait Derivation4P[F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor] {
    val axo = AndXor4[A1, A2, A3, A4]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.map(axo.apply[TC, F])(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.contramap(axo.divide[TC, F])(iso.to(_))
  }

  trait Derivation5P[F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor] {
    val axo = AndXor5[A1, A2, A3, A4, A5]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.map(axo.apply[TC, F])(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.contramap(axo.divide[TC, F])(iso.to(_))
  }

  trait Derivation6P[F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor] {
    val axo = AndXor6[A1, A2, A3, A4, A5, A6]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.map(axo.apply[TC, F])(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.contramap(axo.divide[TC, F])(iso.to(_))
  }

  trait Derivation7P[F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor] {
    val axo = AndXor7[A1, A2, A3, A4, A5, A6, A7]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.map(axo.apply[TC, F])(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.contramap(axo.divide[TC, F])(iso.to(_))
  }

  trait Derivation8P[F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor] {
    val axo = AndXor8[A1, A2, A3, A4, A5, A6, A7, A8]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.map(axo.apply[TC, F])(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.contramap(axo.divide[TC, F])(iso.to(_))
  }

  trait Derivation9P[F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor] {
    val axo = AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.map(axo.apply[TC, F])(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.contramap(axo.divide[TC, F])(iso.to(_))
  }

  trait Derivation10P[F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor] {
    val axo = AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.map(axo.apply[TC, F])(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.contramap(axo.divide[TC, F])(iso.to(_))
  }

  trait Derivation11P[F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor] {
    val axo = AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.map(axo.apply[TC, F])(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.contramap(axo.divide[TC, F])(iso.to(_))
  }

  trait Derivation12P[F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor] {
    val axo = AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.map(axo.apply[TC, F])(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.contramap(axo.divide[TC, F])(iso.to(_))
  }

  trait Derivation13P[F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor] {
    val axo = AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.map(axo.apply[TC, F])(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.contramap(axo.divide[TC, F])(iso.to(_))
  }

  trait Derivation14P[F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor] {
    val axo = AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.map(axo.apply[TC, F])(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.contramap(axo.divide[TC, F])(iso.to(_))
  }

  trait Derivation15P[F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor] {
    val axo = AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.map(axo.apply[TC, F])(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.contramap(axo.divide[TC, F])(iso.to(_))
  }

  trait Derivation16P[F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor] {
    val axo = AndXor16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.map(axo.apply[TC, F])(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.contramap(axo.divide[TC, F])(iso.to(_))
  }

  trait Derivation17P[F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor, A17 <: AndXor] {
    val axo = AndXor17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.map(axo.apply[TC, F])(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.contramap(axo.divide[TC, F])(iso.to(_))
  }

  trait Derivation18P[F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor, A17 <: AndXor, A18 <: AndXor] {
    val axo = AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.map(axo.apply[TC, F])(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.contramap(axo.divide[TC, F])(iso.to(_))
  }

  trait Derivation19P[F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor, A17 <: AndXor, A18 <: AndXor, A19 <: AndXor] {
    val axo = AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.map(axo.apply[TC, F])(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.contramap(axo.divide[TC, F])(iso.to(_))
  }

  trait Derivation20P[F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor, A17 <: AndXor, A18 <: AndXor, A19 <: AndXor, A20 <: AndXor] {
    val axo = AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.map(axo.apply[TC, F])(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.contramap(axo.divide[TC, F])(iso.to(_))
  }

  trait Derivation21P[F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor, A17 <: AndXor, A18 <: AndXor, A19 <: AndXor, A20 <: AndXor, A21 <: AndXor] {
    val axo = AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.map(axo.apply[TC, F])(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], d: DerivingProd[axo.Prod, F, TC]): TC[A] =
      F.contramap(axo.divide[TC, F])(iso.to(_))
  }

  trait Derivation22P[F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor, A17 <: AndXor, A18 <: AndXor, A19 <: AndXor, A20 <: AndXor, A21 <: AndXor, A22 <: AndXor] {
    val axo = AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

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

    implicit class Derivation3POps[A, F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor](iso: A <=> AndXor3[A1, A2, A3]#Prod[F]) {
      val derive = new Derivation3P[F, A1, A2, A3] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation4POps[A, F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor](iso: A <=> AndXor4[A1, A2, A3, A4]#Prod[F]) {
      val derive = new Derivation4P[F, A1, A2, A3, A4] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation5POps[A, F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor](iso: A <=> AndXor5[A1, A2, A3, A4, A5]#Prod[F]) {
      val derive = new Derivation5P[F, A1, A2, A3, A4, A5] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation6POps[A, F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor](iso: A <=> AndXor6[A1, A2, A3, A4, A5, A6]#Prod[F]) {
      val derive = new Derivation6P[F, A1, A2, A3, A4, A5, A6] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation7POps[A, F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor](iso: A <=> AndXor7[A1, A2, A3, A4, A5, A6, A7]#Prod[F]) {
      val derive = new Derivation7P[F, A1, A2, A3, A4, A5, A6, A7] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation8POps[A, F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor](iso: A <=> AndXor8[A1, A2, A3, A4, A5, A6, A7, A8]#Prod[F]) {
      val derive = new Derivation8P[F, A1, A2, A3, A4, A5, A6, A7, A8] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation9POps[A, F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor](iso: A <=> AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9]#Prod[F]) {
      val derive = new Derivation9P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation10POps[A, F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor](iso: A <=> AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]#Prod[F]) {
      val derive = new Derivation10P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation11POps[A, F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor](iso: A <=> AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]#Prod[F]) {
      val derive = new Derivation11P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation12POps[A, F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor](iso: A <=> AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod[F]) {
      val derive = new Derivation12P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation13POps[A, F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor](iso: A <=> AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]#Prod[F]) {
      val derive = new Derivation13P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation14POps[A, F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor](iso: A <=> AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Prod[F]) {
      val derive = new Derivation14P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation15POps[A, F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor](iso: A <=> AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]#Prod[F]) {
      val derive = new Derivation15P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation16POps[A, F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor](iso: A <=> AndXor16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]#Prod[F]) {
      val derive = new Derivation16P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation17POps[A, F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor, A17 <: AndXor](iso: A <=> AndXor17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]#Prod[F]) {
      val derive = new Derivation17P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation18POps[A, F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor, A17 <: AndXor, A18 <: AndXor](iso: A <=> AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]#Prod[F]) {
      val derive = new Derivation18P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation19POps[A, F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor, A17 <: AndXor, A18 <: AndXor, A19 <: AndXor](iso: A <=> AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]#Prod[F]) {
      val derive = new Derivation19P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation20POps[A, F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor, A17 <: AndXor, A18 <: AndXor, A19 <: AndXor, A20 <: AndXor](iso: A <=> AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]#Prod[F]) {
      val derive = new Derivation20P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation21POps[A, F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor, A17 <: AndXor, A18 <: AndXor, A19 <: AndXor, A20 <: AndXor, A21 <: AndXor](iso: A <=> AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]#Prod[F]) {
      val derive = new Derivation21P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, F, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation22POps[A, F[_], A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor, A17 <: AndXor, A18 <: AndXor, A19 <: AndXor, A20 <: AndXor, A21 <: AndXor, A22 <: AndXor](iso: A <=> AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]#Prod[F]) {
      val derive = new Derivation22P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {}

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

    implicit class Derivation3POps[A, A1 <: AndXor, A2 <: AndXor, A3 <: AndXor](iso: A <=> AndXor3[A1, A2, A3]#Prod[Id]) {
      val derive = new Derivation3P[Id, A1, A2, A3] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation4POps[A, A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor](iso: A <=> AndXor4[A1, A2, A3, A4]#Prod[Id]) {
      val derive = new Derivation4P[Id, A1, A2, A3, A4] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation5POps[A, A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor](iso: A <=> AndXor5[A1, A2, A3, A4, A5]#Prod[Id]) {
      val derive = new Derivation5P[Id, A1, A2, A3, A4, A5] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation6POps[A, A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor](iso: A <=> AndXor6[A1, A2, A3, A4, A5, A6]#Prod[Id]) {
      val derive = new Derivation6P[Id, A1, A2, A3, A4, A5, A6] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation7POps[A, A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor](iso: A <=> AndXor7[A1, A2, A3, A4, A5, A6, A7]#Prod[Id]) {
      val derive = new Derivation7P[Id, A1, A2, A3, A4, A5, A6, A7] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation8POps[A, A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor](iso: A <=> AndXor8[A1, A2, A3, A4, A5, A6, A7, A8]#Prod[Id]) {
      val derive = new Derivation8P[Id, A1, A2, A3, A4, A5, A6, A7, A8] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation9POps[A, A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor](iso: A <=> AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9]#Prod[Id]) {
      val derive = new Derivation9P[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation10POps[A, A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor](iso: A <=> AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]#Prod[Id]) {
      val derive = new Derivation10P[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation11POps[A, A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor](iso: A <=> AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]#Prod[Id]) {
      val derive = new Derivation11P[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation12POps[A, A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor](iso: A <=> AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod[Id]) {
      val derive = new Derivation12P[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation13POps[A, A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor](iso: A <=> AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]#Prod[Id]) {
      val derive = new Derivation13P[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation14POps[A, A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor](iso: A <=> AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Prod[Id]) {
      val derive = new Derivation14P[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation15POps[A, A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor](iso: A <=> AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]#Prod[Id]) {
      val derive = new Derivation15P[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation16POps[A, A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor](iso: A <=> AndXor16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]#Prod[Id]) {
      val derive = new Derivation16P[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation17POps[A, A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor, A17 <: AndXor](iso: A <=> AndXor17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]#Prod[Id]) {
      val derive = new Derivation17P[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation18POps[A, A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor, A17 <: AndXor, A18 <: AndXor](iso: A <=> AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]#Prod[Id]) {
      val derive = new Derivation18P[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation19POps[A, A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor, A17 <: AndXor, A18 <: AndXor, A19 <: AndXor](iso: A <=> AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]#Prod[Id]) {
      val derive = new Derivation19P[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation20POps[A, A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor, A17 <: AndXor, A18 <: AndXor, A19 <: AndXor, A20 <: AndXor](iso: A <=> AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]#Prod[Id]) {
      val derive = new Derivation20P[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation21POps[A, A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor, A17 <: AndXor, A18 <: AndXor, A19 <: AndXor, A20 <: AndXor, A21 <: AndXor](iso: A <=> AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]#Prod[Id]) {
      val derive = new Derivation21P[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation22POps[A, A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor, A17 <: AndXor, A18 <: AndXor, A19 <: AndXor, A20 <: AndXor, A21 <: AndXor, A22 <: AndXor](iso: A <=> AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]#Prod[Id]) {
      val derive = new Derivation22P[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], d: DerivingProd[derive.axo.Prod, Id, TC]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

  }
}
