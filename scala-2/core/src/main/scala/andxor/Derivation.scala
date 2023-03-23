package andxor

import cats.Apply
import monocle.Iso

object derivation {

  trait Derivation1C[A1] {
    val axo = AndXor1[A1]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation1P[A1] {
    val axo = AndXor1[A1]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  trait Derivation2C[A1, A2] {
    val axo = AndXor2[A1, A2]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation2P[A1, A2] {
    val axo = AndXor2[A1, A2]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  trait Derivation3C[A1, A2, A3] {
    val axo = AndXor3[A1, A2, A3]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation3P[A1, A2, A3] {
    val axo = AndXor3[A1, A2, A3]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  trait Derivation4C[A1, A2, A3, A4] {
    val axo = AndXor4[A1, A2, A3, A4]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation4P[A1, A2, A3, A4] {
    val axo = AndXor4[A1, A2, A3, A4]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  trait Derivation5C[A1, A2, A3, A4, A5] {
    val axo = AndXor5[A1, A2, A3, A4, A5]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation5P[A1, A2, A3, A4, A5] {
    val axo = AndXor5[A1, A2, A3, A4, A5]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  trait Derivation6C[A1, A2, A3, A4, A5, A6] {
    val axo = AndXor6[A1, A2, A3, A4, A5, A6]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation6P[A1, A2, A3, A4, A5, A6] {
    val axo = AndXor6[A1, A2, A3, A4, A5, A6]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  trait Derivation7C[A1, A2, A3, A4, A5, A6, A7] {
    val axo = AndXor7[A1, A2, A3, A4, A5, A6, A7]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation7P[A1, A2, A3, A4, A5, A6, A7] {
    val axo = AndXor7[A1, A2, A3, A4, A5, A6, A7]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  trait Derivation8C[A1, A2, A3, A4, A5, A6, A7, A8] {
    val axo = AndXor8[A1, A2, A3, A4, A5, A6, A7, A8]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation8P[A1, A2, A3, A4, A5, A6, A7, A8] {
    val axo = AndXor8[A1, A2, A3, A4, A5, A6, A7, A8]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  trait Derivation9C[A1, A2, A3, A4, A5, A6, A7, A8, A9] {
    val axo = AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation9P[A1, A2, A3, A4, A5, A6, A7, A8, A9] {
    val axo = AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  trait Derivation10C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {
    val axo = AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation10P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {
    val axo = AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  trait Derivation11C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {
    val axo = AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation11P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {
    val axo = AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  trait Derivation12C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {
    val axo = AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation12P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {
    val axo = AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  trait Derivation13C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {
    val axo = AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation13P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {
    val axo = AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  trait Derivation14C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {
    val axo = AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation14P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {
    val axo = AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  trait Derivation15C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] {
    val axo = AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation15P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] {
    val axo = AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  trait Derivation16C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] {
    val axo = AndXor16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation16P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] {
    val axo = AndXor16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  trait Derivation17C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] {
    val axo = AndXor17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation17P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] {
    val axo = AndXor17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  trait Derivation18C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {
    val axo = AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation18P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {
    val axo = AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  trait Derivation19C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] {
    val axo = AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation19P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] {
    val axo = AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  trait Derivation20C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {
    val axo = AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation20P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {
    val axo = AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  trait Derivation21C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {
    val axo = AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]], t20: TC[F[A21]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]], t20: TC[F[A21]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation21P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {
    val axo = AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]], t20: TC[F[A21]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]], t20: TC[F[A21]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  trait Derivation22C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {
    val axo = AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]], t20: TC[F[A21]], t21: TC[F[A22]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Cop[F]])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]], t20: TC[F[A21]], t21: TC[F[A22]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.get(_))
  }

  trait Derivation22P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {
    val axo = AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    def deriveCovariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]], t20: TC[F[A21]], t21: TC[F[A22]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.reverseGet(_))

    def deriveContravariant[TC[_], F[_], A](iso: Iso[A, axo.Prod[F]])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]], t20: TC[F[A21]], t21: TC[F[A22]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.get(_))
  }

  object ops {

    implicit class Derivation1COps[A, A1, F[_]](iso: Iso[A, AndXor1[A1]#Cop[F]]) {
      val derive = new Derivation1C[A1] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation1POps[A, A1, F[_]](iso: Iso[A, AndXor1[A1]#Prod[F]]) {
      val derive = new Derivation1P[A1] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation2COps[A, A1, A2, F[_]](iso: Iso[A, AndXor2[A1, A2]#Cop[F]]) {
      val derive = new Derivation2C[A1, A2] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation2POps[A, A1, A2, F[_]](iso: Iso[A, AndXor2[A1, A2]#Prod[F]]) {
      val derive = new Derivation2P[A1, A2] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation3COps[A, A1, A2, A3, F[_]](iso: Iso[A, AndXor3[A1, A2, A3]#Cop[F]]) {
      val derive = new Derivation3C[A1, A2, A3] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation3POps[A, A1, A2, A3, F[_]](iso: Iso[A, AndXor3[A1, A2, A3]#Prod[F]]) {
      val derive = new Derivation3P[A1, A2, A3] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation4COps[A, A1, A2, A3, A4, F[_]](iso: Iso[A, AndXor4[A1, A2, A3, A4]#Cop[F]]) {
      val derive = new Derivation4C[A1, A2, A3, A4] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation4POps[A, A1, A2, A3, A4, F[_]](iso: Iso[A, AndXor4[A1, A2, A3, A4]#Prod[F]]) {
      val derive = new Derivation4P[A1, A2, A3, A4] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation5COps[A, A1, A2, A3, A4, A5, F[_]](iso: Iso[A, AndXor5[A1, A2, A3, A4, A5]#Cop[F]]) {
      val derive = new Derivation5C[A1, A2, A3, A4, A5] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation5POps[A, A1, A2, A3, A4, A5, F[_]](iso: Iso[A, AndXor5[A1, A2, A3, A4, A5]#Prod[F]]) {
      val derive = new Derivation5P[A1, A2, A3, A4, A5] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation6COps[A, A1, A2, A3, A4, A5, A6, F[_]](iso: Iso[A, AndXor6[A1, A2, A3, A4, A5, A6]#Cop[F]]) {
      val derive = new Derivation6C[A1, A2, A3, A4, A5, A6] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation6POps[A, A1, A2, A3, A4, A5, A6, F[_]](iso: Iso[A, AndXor6[A1, A2, A3, A4, A5, A6]#Prod[F]]) {
      val derive = new Derivation6P[A1, A2, A3, A4, A5, A6] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation7COps[A, A1, A2, A3, A4, A5, A6, A7, F[_]](iso: Iso[A, AndXor7[A1, A2, A3, A4, A5, A6, A7]#Cop[F]]) {
      val derive = new Derivation7C[A1, A2, A3, A4, A5, A6, A7] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation7POps[A, A1, A2, A3, A4, A5, A6, A7, F[_]](iso: Iso[A, AndXor7[A1, A2, A3, A4, A5, A6, A7]#Prod[F]]) {
      val derive = new Derivation7P[A1, A2, A3, A4, A5, A6, A7] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation8COps[A, A1, A2, A3, A4, A5, A6, A7, A8, F[_]](iso: Iso[A, AndXor8[A1, A2, A3, A4, A5, A6, A7, A8]#Cop[F]]) {
      val derive = new Derivation8C[A1, A2, A3, A4, A5, A6, A7, A8] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation8POps[A, A1, A2, A3, A4, A5, A6, A7, A8, F[_]](iso: Iso[A, AndXor8[A1, A2, A3, A4, A5, A6, A7, A8]#Prod[F]]) {
      val derive = new Derivation8P[A1, A2, A3, A4, A5, A6, A7, A8] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation9COps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, F[_]](iso: Iso[A, AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9]#Cop[F]]) {
      val derive = new Derivation9C[A1, A2, A3, A4, A5, A6, A7, A8, A9] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation9POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, F[_]](iso: Iso[A, AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9]#Prod[F]]) {
      val derive = new Derivation9P[A1, A2, A3, A4, A5, A6, A7, A8, A9] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation10COps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, F[_]](iso: Iso[A, AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]#Cop[F]]) {
      val derive = new Derivation10C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation10POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, F[_]](iso: Iso[A, AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]#Prod[F]]) {
      val derive = new Derivation10P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation11COps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, F[_]](iso: Iso[A, AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]#Cop[F]]) {
      val derive = new Derivation11C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation11POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, F[_]](iso: Iso[A, AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]#Prod[F]]) {
      val derive = new Derivation11P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation12COps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, F[_]](iso: Iso[A, AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Cop[F]]) {
      val derive = new Derivation12C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation12POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, F[_]](iso: Iso[A, AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod[F]]) {
      val derive = new Derivation12P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation13COps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, F[_]](iso: Iso[A, AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]#Cop[F]]) {
      val derive = new Derivation13C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation13POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, F[_]](iso: Iso[A, AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]#Prod[F]]) {
      val derive = new Derivation13P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation14COps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, F[_]](iso: Iso[A, AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Cop[F]]) {
      val derive = new Derivation14C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation14POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, F[_]](iso: Iso[A, AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Prod[F]]) {
      val derive = new Derivation14P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation15COps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, F[_]](iso: Iso[A, AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]#Cop[F]]) {
      val derive = new Derivation15C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation15POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, F[_]](iso: Iso[A, AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]#Prod[F]]) {
      val derive = new Derivation15P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation16COps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, F[_]](iso: Iso[A, AndXor16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]#Cop[F]]) {
      val derive = new Derivation16C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation16POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, F[_]](iso: Iso[A, AndXor16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]#Prod[F]]) {
      val derive = new Derivation16P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation17COps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, F[_]](iso: Iso[A, AndXor17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]#Cop[F]]) {
      val derive = new Derivation17C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation17POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, F[_]](iso: Iso[A, AndXor17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]#Prod[F]]) {
      val derive = new Derivation17P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation18COps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, F[_]](iso: Iso[A, AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]#Cop[F]]) {
      val derive = new Derivation18C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation18POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, F[_]](iso: Iso[A, AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]#Prod[F]]) {
      val derive = new Derivation18P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation19COps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, F[_]](iso: Iso[A, AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]#Cop[F]]) {
      val derive = new Derivation19C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation19POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, F[_]](iso: Iso[A, AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]#Prod[F]]) {
      val derive = new Derivation19P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation20COps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, F[_]](iso: Iso[A, AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]#Cop[F]]) {
      val derive = new Derivation20C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation20POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, F[_]](iso: Iso[A, AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]#Prod[F]]) {
      val derive = new Derivation20P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation21COps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, F[_]](iso: Iso[A, AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]#Cop[F]]) {
      val derive = new Derivation21C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]], t20: TC[F[A21]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]], t20: TC[F[A21]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation21POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, F[_]](iso: Iso[A, AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]#Prod[F]]) {
      val derive = new Derivation21P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]], t20: TC[F[A21]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]], t20: TC[F[A21]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation22COps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, F[_]](iso: Iso[A, AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]#Cop[F]]) {
      val derive = new Derivation22C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]], t20: TC[F[A21]], t21: TC[F[A22]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]], t20: TC[F[A21]], t21: TC[F[A22]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation22POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, F[_]](iso: Iso[A, AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]#Prod[F]]) {
      val derive = new Derivation22P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]], t20: TC[F[A21]], t21: TC[F[A22]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]], t6: TC[F[A7]], t7: TC[F[A8]], t8: TC[F[A9]], t9: TC[F[A10]], t10: TC[F[A11]], t11: TC[F[A12]], t12: TC[F[A13]], t13: TC[F[A14]], t14: TC[F[A15]], t15: TC[F[A16]], t16: TC[F[A17]], t17: TC[F[A18]], t18: TC[F[A19]], t19: TC[F[A20]], t20: TC[F[A21]], t21: TC[F[A22]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

  }
}
