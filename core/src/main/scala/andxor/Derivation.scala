package andxor

import scalaz.Apply
import scalaz.Isomorphism.<=>

object derivation {

  trait Derivation2C[A1[_[_]], A2[_[_]]] {
    val axo = AndXorNested2[A1, A2]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation2P[A1[_[_]], A2[_[_]]] {
    val axo = AndXorNested2[A1, A2]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation3C[A1[_[_]], A2[_[_]], A3[_[_]]] {
    val axo = AndXorNested3[A1, A2, A3]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation3P[A1[_[_]], A2[_[_]], A3[_[_]]] {
    val axo = AndXorNested3[A1, A2, A3]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation4C[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]]] {
    val axo = AndXorNested4[A1, A2, A3, A4]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation4P[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]]] {
    val axo = AndXorNested4[A1, A2, A3, A4]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation5C[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]]] {
    val axo = AndXorNested5[A1, A2, A3, A4, A5]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation5P[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]]] {
    val axo = AndXorNested5[A1, A2, A3, A4, A5]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation6C[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]]] {
    val axo = AndXorNested6[A1, A2, A3, A4, A5, A6]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation6P[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]]] {
    val axo = AndXorNested6[A1, A2, A3, A4, A5, A6]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation7C[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]]] {
    val axo = AndXorNested7[A1, A2, A3, A4, A5, A6, A7]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation7P[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]]] {
    val axo = AndXorNested7[A1, A2, A3, A4, A5, A6, A7]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation8C[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]]] {
    val axo = AndXorNested8[A1, A2, A3, A4, A5, A6, A7, A8]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation8P[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]]] {
    val axo = AndXorNested8[A1, A2, A3, A4, A5, A6, A7, A8]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation9C[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]]] {
    val axo = AndXorNested9[A1, A2, A3, A4, A5, A6, A7, A8, A9]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation9P[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]]] {
    val axo = AndXorNested9[A1, A2, A3, A4, A5, A6, A7, A8, A9]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation10C[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]]] {
    val axo = AndXorNested10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation10P[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]]] {
    val axo = AndXorNested10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation11C[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]]] {
    val axo = AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation11P[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]]] {
    val axo = AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation12C[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]]] {
    val axo = AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation12P[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]]] {
    val axo = AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation13C[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]]] {
    val axo = AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation13P[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]]] {
    val axo = AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation14C[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]]] {
    val axo = AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation14P[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]]] {
    val axo = AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation15C[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]]] {
    val axo = AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation15P[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]]] {
    val axo = AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation16C[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]]] {
    val axo = AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation16P[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]]] {
    val axo = AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation17C[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]]] {
    val axo = AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation17P[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]]] {
    val axo = AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation18C[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]]] {
    val axo = AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation18P[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]]] {
    val axo = AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation19C[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]]] {
    val axo = AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation19P[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]]] {
    val axo = AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation20C[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]]] {
    val axo = AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation20P[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]]] {
    val axo = AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation21C[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]]] {
    val axo = AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]], t20: TC[A21[F]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]], t20: TC[A21[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation21P[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]]] {
    val axo = AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]], t20: TC[A21[F]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]], t20: TC[A21[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation22C[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]], A22[_[_]]] {
    val axo = AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]], t20: TC[A21[F]], t21: TC[A22[F]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]], t20: TC[A21[F]], t21: TC[A22[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation22P[A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]], A22[_[_]]] {
    val axo = AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]], t20: TC[A21[F]], t21: TC[A22[F]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]], t20: TC[A21[F]], t21: TC[A22[F]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  object ops {

    implicit class Derivation2COps[A, A1[_[_]], A2[_[_]], F[_]](iso: A <=> AndXorNested2[A1, A2]#Cop[F]) {
      val derive = new Derivation2C[A1, A2] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation2POps[A, A1[_[_]], A2[_[_]], F[_]](iso: A <=> AndXorNested2[A1, A2]#Prod[F]) {
      val derive = new Derivation2P[A1, A2] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation3COps[A, A1[_[_]], A2[_[_]], A3[_[_]], F[_]](iso: A <=> AndXorNested3[A1, A2, A3]#Cop[F]) {
      val derive = new Derivation3C[A1, A2, A3] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation3POps[A, A1[_[_]], A2[_[_]], A3[_[_]], F[_]](iso: A <=> AndXorNested3[A1, A2, A3]#Prod[F]) {
      val derive = new Derivation3P[A1, A2, A3] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation4COps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], F[_]](iso: A <=> AndXorNested4[A1, A2, A3, A4]#Cop[F]) {
      val derive = new Derivation4C[A1, A2, A3, A4] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation4POps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], F[_]](iso: A <=> AndXorNested4[A1, A2, A3, A4]#Prod[F]) {
      val derive = new Derivation4P[A1, A2, A3, A4] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation5COps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], F[_]](iso: A <=> AndXorNested5[A1, A2, A3, A4, A5]#Cop[F]) {
      val derive = new Derivation5C[A1, A2, A3, A4, A5] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation5POps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], F[_]](iso: A <=> AndXorNested5[A1, A2, A3, A4, A5]#Prod[F]) {
      val derive = new Derivation5P[A1, A2, A3, A4, A5] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation6COps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], F[_]](iso: A <=> AndXorNested6[A1, A2, A3, A4, A5, A6]#Cop[F]) {
      val derive = new Derivation6C[A1, A2, A3, A4, A5, A6] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation6POps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], F[_]](iso: A <=> AndXorNested6[A1, A2, A3, A4, A5, A6]#Prod[F]) {
      val derive = new Derivation6P[A1, A2, A3, A4, A5, A6] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation7COps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], F[_]](iso: A <=> AndXorNested7[A1, A2, A3, A4, A5, A6, A7]#Cop[F]) {
      val derive = new Derivation7C[A1, A2, A3, A4, A5, A6, A7] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation7POps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], F[_]](iso: A <=> AndXorNested7[A1, A2, A3, A4, A5, A6, A7]#Prod[F]) {
      val derive = new Derivation7P[A1, A2, A3, A4, A5, A6, A7] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation8COps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], F[_]](iso: A <=> AndXorNested8[A1, A2, A3, A4, A5, A6, A7, A8]#Cop[F]) {
      val derive = new Derivation8C[A1, A2, A3, A4, A5, A6, A7, A8] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation8POps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], F[_]](iso: A <=> AndXorNested8[A1, A2, A3, A4, A5, A6, A7, A8]#Prod[F]) {
      val derive = new Derivation8P[A1, A2, A3, A4, A5, A6, A7, A8] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation9COps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], F[_]](iso: A <=> AndXorNested9[A1, A2, A3, A4, A5, A6, A7, A8, A9]#Cop[F]) {
      val derive = new Derivation9C[A1, A2, A3, A4, A5, A6, A7, A8, A9] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation9POps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], F[_]](iso: A <=> AndXorNested9[A1, A2, A3, A4, A5, A6, A7, A8, A9]#Prod[F]) {
      val derive = new Derivation9P[A1, A2, A3, A4, A5, A6, A7, A8, A9] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation10COps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], F[_]](iso: A <=> AndXorNested10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]#Cop[F]) {
      val derive = new Derivation10C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation10POps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], F[_]](iso: A <=> AndXorNested10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]#Prod[F]) {
      val derive = new Derivation10P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation11COps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], F[_]](iso: A <=> AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]#Cop[F]) {
      val derive = new Derivation11C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation11POps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], F[_]](iso: A <=> AndXorNested11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]#Prod[F]) {
      val derive = new Derivation11P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation12COps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], F[_]](iso: A <=> AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Cop[F]) {
      val derive = new Derivation12C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation12POps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], F[_]](iso: A <=> AndXorNested12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod[F]) {
      val derive = new Derivation12P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation13COps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], F[_]](iso: A <=> AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]#Cop[F]) {
      val derive = new Derivation13C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation13POps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], F[_]](iso: A <=> AndXorNested13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]#Prod[F]) {
      val derive = new Derivation13P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation14COps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], F[_]](iso: A <=> AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Cop[F]) {
      val derive = new Derivation14C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation14POps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], F[_]](iso: A <=> AndXorNested14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Prod[F]) {
      val derive = new Derivation14P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation15COps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], F[_]](iso: A <=> AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]#Cop[F]) {
      val derive = new Derivation15C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation15POps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], F[_]](iso: A <=> AndXorNested15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]#Prod[F]) {
      val derive = new Derivation15P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation16COps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], F[_]](iso: A <=> AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]#Cop[F]) {
      val derive = new Derivation16C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation16POps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], F[_]](iso: A <=> AndXorNested16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]#Prod[F]) {
      val derive = new Derivation16P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation17COps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], F[_]](iso: A <=> AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]#Cop[F]) {
      val derive = new Derivation17C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation17POps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], F[_]](iso: A <=> AndXorNested17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]#Prod[F]) {
      val derive = new Derivation17P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation18COps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], F[_]](iso: A <=> AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]#Cop[F]) {
      val derive = new Derivation18C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation18POps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], F[_]](iso: A <=> AndXorNested18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]#Prod[F]) {
      val derive = new Derivation18P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation19COps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], F[_]](iso: A <=> AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]#Cop[F]) {
      val derive = new Derivation19C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation19POps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], F[_]](iso: A <=> AndXorNested19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]#Prod[F]) {
      val derive = new Derivation19P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation20COps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], F[_]](iso: A <=> AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]#Cop[F]) {
      val derive = new Derivation20C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation20POps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], F[_]](iso: A <=> AndXorNested20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]#Prod[F]) {
      val derive = new Derivation20P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation21COps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]], F[_]](iso: A <=> AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]#Cop[F]) {
      val derive = new Derivation21C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]], t20: TC[A21[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]], t20: TC[A21[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation21POps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]], F[_]](iso: A <=> AndXorNested21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]#Prod[F]) {
      val derive = new Derivation21P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]], t20: TC[A21[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]], t20: TC[A21[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation22COps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]], A22[_[_]], F[_]](iso: A <=> AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]#Cop[F]) {
      val derive = new Derivation22C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]], t20: TC[A21[F]], t21: TC[A22[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]], t20: TC[A21[F]], t21: TC[A22[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation22POps[A, A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]], A22[_[_]], F[_]](iso: A <=> AndXorNested22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]#Prod[F]) {
      val derive = new Derivation22P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]], t20: TC[A21[F]], t21: TC[A22[F]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[A1[F]], t1: TC[A2[F]], t2: TC[A3[F]], t3: TC[A4[F]], t4: TC[A5[F]], t5: TC[A6[F]], t6: TC[A7[F]], t7: TC[A8[F]], t8: TC[A9[F]], t9: TC[A10[F]], t10: TC[A11[F]], t11: TC[A12[F]], t12: TC[A13[F]], t13: TC[A14[F]], t14: TC[A15[F]], t15: TC[A16[F]], t16: TC[A17[F]], t17: TC[A18[F]], t18: TC[A19[F]], t19: TC[A20[F]], t20: TC[A21[F]], t21: TC[A22[F]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

  }
}
