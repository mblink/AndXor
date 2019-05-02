package andxor

import scalaz.Apply
import scalaz.Isomorphism.<=>

object derivation {

  trait Derivation1P[F[_], A1] {
    val axo = AndXorK1[F, A1]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Apply[TC], a0: TC[F[A1]]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Divide[TC], a0: TC[F[A1]]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation1PId[A1] {
    val axo = AndXor1[A1]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Apply[TC], a0: TC[A1]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Divide[TC], a0: TC[A1]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation2P[F[_], A1, A2] {
    val axo = AndXorK2[F, A1, A2]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Apply[TC], a0: TC[F[A1]], a1: TC[F[A2]]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Divide[TC], a0: TC[F[A1]], a1: TC[F[A2]]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation2PId[A1, A2] {
    val axo = AndXor2[A1, A2]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Apply[TC], a0: TC[A1], a1: TC[A2]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Divide[TC], a0: TC[A1], a1: TC[A2]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation3P[F[_], A1, A2, A3] {
    val axo = AndXorK3[F, A1, A2, A3]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Apply[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Divide[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation3PId[A1, A2, A3] {
    val axo = AndXor3[A1, A2, A3]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Apply[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Divide[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation4P[F[_], A1, A2, A3, A4] {
    val axo = AndXorK4[F, A1, A2, A3, A4]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Apply[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Divide[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation4PId[A1, A2, A3, A4] {
    val axo = AndXor4[A1, A2, A3, A4]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Apply[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Divide[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation5P[F[_], A1, A2, A3, A4, A5] {
    val axo = AndXorK5[F, A1, A2, A3, A4, A5]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Apply[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Divide[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation5PId[A1, A2, A3, A4, A5] {
    val axo = AndXor5[A1, A2, A3, A4, A5]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Apply[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Divide[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation6P[F[_], A1, A2, A3, A4, A5, A6] {
    val axo = AndXorK6[F, A1, A2, A3, A4, A5, A6]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Apply[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]], a5: TC[F[A6]]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Divide[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]], a5: TC[F[A6]]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation6PId[A1, A2, A3, A4, A5, A6] {
    val axo = AndXor6[A1, A2, A3, A4, A5, A6]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Apply[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Divide[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation7P[F[_], A1, A2, A3, A4, A5, A6, A7] {
    val axo = AndXorK7[F, A1, A2, A3, A4, A5, A6, A7]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Apply[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]], a5: TC[F[A6]], a6: TC[F[A7]]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Divide[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]], a5: TC[F[A6]], a6: TC[F[A7]]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation7PId[A1, A2, A3, A4, A5, A6, A7] {
    val axo = AndXor7[A1, A2, A3, A4, A5, A6, A7]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Apply[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Divide[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation8P[F[_], A1, A2, A3, A4, A5, A6, A7, A8] {
    val axo = AndXorK8[F, A1, A2, A3, A4, A5, A6, A7, A8]

    def deriveCovariant[TC[_], A](
        iso: A <=> axo.Prod
    )(implicit F: Apply[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]], a5: TC[F[A6]], a6: TC[F[A7]], a7: TC[F[A8]]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](
        iso: A <=> axo.Prod
    )(implicit F: Divide[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]], a5: TC[F[A6]], a6: TC[F[A7]], a7: TC[F[A8]]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation8PId[A1, A2, A3, A4, A5, A6, A7, A8] {
    val axo = AndXor8[A1, A2, A3, A4, A5, A6, A7, A8]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Apply[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7], a7: TC[A8]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Divide[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7], a7: TC[A8]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation9P[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9] {
    val axo = AndXorK9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    def deriveCovariant[TC[_], A](
        iso: A <=> axo.Prod
    )(implicit F: Apply[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]], a5: TC[F[A6]], a6: TC[F[A7]], a7: TC[F[A8]], a8: TC[F[A9]]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](
        iso: A <=> axo.Prod
    )(implicit F: Divide[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]], a5: TC[F[A6]], a6: TC[F[A7]], a7: TC[F[A8]], a8: TC[F[A9]]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation9PId[A1, A2, A3, A4, A5, A6, A7, A8, A9] {
    val axo = AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Apply[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7], a7: TC[A8], a8: TC[A9]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(implicit F: Divide[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7], a7: TC[A8], a8: TC[A9]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation10P[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {
    val axo = AndXorK10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    def deriveCovariant[TC[_], A](
        iso: A <=> axo.Prod
    )(implicit F: Apply[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]], a5: TC[F[A6]], a6: TC[F[A7]], a7: TC[F[A8]], a8: TC[F[A9]], a9: TC[F[A10]]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](
        iso: A <=> axo.Prod
    )(implicit F: Divide[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]], a5: TC[F[A6]], a6: TC[F[A7]], a7: TC[F[A8]], a8: TC[F[A9]], a9: TC[F[A10]]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation10PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {
    val axo = AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    def deriveCovariant[TC[_], A](
        iso: A <=> axo.Prod
    )(implicit F: Apply[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7], a7: TC[A8], a8: TC[A9], a9: TC[A10]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](
        iso: A <=> axo.Prod
    )(implicit F: Divide[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7], a7: TC[A8], a8: TC[A9], a9: TC[A10]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation11P[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {
    val axo = AndXorK11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    def deriveCovariant[TC[_], A](
        iso: A <=> axo.Prod
    )(implicit F: Apply[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]], a5: TC[F[A6]], a6: TC[F[A7]], a7: TC[F[A8]], a8: TC[F[A9]], a9: TC[F[A10]], a10: TC[F[A11]])
        : TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](
        iso: A <=> axo.Prod
    )(implicit F: Divide[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]], a5: TC[F[A6]], a6: TC[F[A7]], a7: TC[F[A8]], a8: TC[F[A9]], a9: TC[F[A10]], a10: TC[F[A11]])
        : TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation11PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {
    val axo = AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    def deriveCovariant[TC[_], A](
        iso: A <=> axo.Prod
    )(implicit F: Apply[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7], a7: TC[A8], a8: TC[A9], a9: TC[A10], a10: TC[A11]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](
        iso: A <=> axo.Prod
    )(implicit F: Divide[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7], a7: TC[A8], a8: TC[A9], a9: TC[A10], a10: TC[A11]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation12P[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {
    val axo = AndXorK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Apply[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]]
    ): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Divide[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]]
    ): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation12PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {
    val axo = AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    def deriveCovariant[TC[_], A](
        iso: A <=> axo.Prod
    )(implicit F: Apply[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7], a7: TC[A8], a8: TC[A9], a9: TC[A10], a10: TC[A11], a11: TC[A12]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](
        iso: A <=> axo.Prod
    )(implicit F: Divide[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7], a7: TC[A8], a8: TC[A9], a9: TC[A10], a10: TC[A11], a11: TC[A12]): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation13P[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {
    val axo = AndXorK13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Apply[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]],
        a12: TC[F[A13]]
    ): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Divide[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]],
        a12: TC[F[A13]]
    ): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation13PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {
    val axo = AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    def deriveCovariant[TC[_], A](
        iso: A <=> axo.Prod
    )(implicit F: Apply[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7], a7: TC[A8], a8: TC[A9], a9: TC[A10], a10: TC[A11], a11: TC[A12], a12: TC[A13]): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](
        iso: A <=> axo.Prod
    )(implicit F: Divide[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7], a7: TC[A8], a8: TC[A9], a9: TC[A10], a10: TC[A11], a11: TC[A12], a12: TC[A13])
        : TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation14P[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {
    val axo = AndXorK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Apply[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]],
        a12: TC[F[A13]],
        a13: TC[F[A14]]
    ): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Divide[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]],
        a12: TC[F[A13]],
        a13: TC[F[A14]]
    ): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation14PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {
    val axo = AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Apply[TC],
        a0: TC[A1],
        a1: TC[A2],
        a2: TC[A3],
        a3: TC[A4],
        a4: TC[A5],
        a5: TC[A6],
        a6: TC[A7],
        a7: TC[A8],
        a8: TC[A9],
        a9: TC[A10],
        a10: TC[A11],
        a11: TC[A12],
        a12: TC[A13],
        a13: TC[A14]
    ): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Divide[TC],
        a0: TC[A1],
        a1: TC[A2],
        a2: TC[A3],
        a3: TC[A4],
        a4: TC[A5],
        a5: TC[A6],
        a6: TC[A7],
        a7: TC[A8],
        a8: TC[A9],
        a9: TC[A10],
        a10: TC[A11],
        a11: TC[A12],
        a12: TC[A13],
        a13: TC[A14]
    ): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation15P[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] {
    val axo = AndXorK15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Apply[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]],
        a12: TC[F[A13]],
        a13: TC[F[A14]],
        a14: TC[F[A15]]
    ): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Divide[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]],
        a12: TC[F[A13]],
        a13: TC[F[A14]],
        a14: TC[F[A15]]
    ): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation15PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] {
    val axo = AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Apply[TC],
        a0: TC[A1],
        a1: TC[A2],
        a2: TC[A3],
        a3: TC[A4],
        a4: TC[A5],
        a5: TC[A6],
        a6: TC[A7],
        a7: TC[A8],
        a8: TC[A9],
        a9: TC[A10],
        a10: TC[A11],
        a11: TC[A12],
        a12: TC[A13],
        a13: TC[A14],
        a14: TC[A15]
    ): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Divide[TC],
        a0: TC[A1],
        a1: TC[A2],
        a2: TC[A3],
        a3: TC[A4],
        a4: TC[A5],
        a5: TC[A6],
        a6: TC[A7],
        a7: TC[A8],
        a8: TC[A9],
        a9: TC[A10],
        a10: TC[A11],
        a11: TC[A12],
        a12: TC[A13],
        a13: TC[A14],
        a14: TC[A15]
    ): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation16P[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] {
    val axo = AndXorK16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Apply[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]],
        a12: TC[F[A13]],
        a13: TC[F[A14]],
        a14: TC[F[A15]],
        a15: TC[F[A16]]
    ): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Divide[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]],
        a12: TC[F[A13]],
        a13: TC[F[A14]],
        a14: TC[F[A15]],
        a15: TC[F[A16]]
    ): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation16PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] {
    val axo = AndXor16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Apply[TC],
        a0: TC[A1],
        a1: TC[A2],
        a2: TC[A3],
        a3: TC[A4],
        a4: TC[A5],
        a5: TC[A6],
        a6: TC[A7],
        a7: TC[A8],
        a8: TC[A9],
        a9: TC[A10],
        a10: TC[A11],
        a11: TC[A12],
        a12: TC[A13],
        a13: TC[A14],
        a14: TC[A15],
        a15: TC[A16]
    ): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Divide[TC],
        a0: TC[A1],
        a1: TC[A2],
        a2: TC[A3],
        a3: TC[A4],
        a4: TC[A5],
        a5: TC[A6],
        a6: TC[A7],
        a7: TC[A8],
        a8: TC[A9],
        a9: TC[A10],
        a10: TC[A11],
        a11: TC[A12],
        a12: TC[A13],
        a13: TC[A14],
        a14: TC[A15],
        a15: TC[A16]
    ): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation17P[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] {
    val axo = AndXorK17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Apply[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]],
        a12: TC[F[A13]],
        a13: TC[F[A14]],
        a14: TC[F[A15]],
        a15: TC[F[A16]],
        a16: TC[F[A17]]
    ): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Divide[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]],
        a12: TC[F[A13]],
        a13: TC[F[A14]],
        a14: TC[F[A15]],
        a15: TC[F[A16]],
        a16: TC[F[A17]]
    ): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation17PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] {
    val axo = AndXor17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Apply[TC],
        a0: TC[A1],
        a1: TC[A2],
        a2: TC[A3],
        a3: TC[A4],
        a4: TC[A5],
        a5: TC[A6],
        a6: TC[A7],
        a7: TC[A8],
        a8: TC[A9],
        a9: TC[A10],
        a10: TC[A11],
        a11: TC[A12],
        a12: TC[A13],
        a13: TC[A14],
        a14: TC[A15],
        a15: TC[A16],
        a16: TC[A17]
    ): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Divide[TC],
        a0: TC[A1],
        a1: TC[A2],
        a2: TC[A3],
        a3: TC[A4],
        a4: TC[A5],
        a5: TC[A6],
        a6: TC[A7],
        a7: TC[A8],
        a8: TC[A9],
        a9: TC[A10],
        a10: TC[A11],
        a11: TC[A12],
        a12: TC[A13],
        a13: TC[A14],
        a14: TC[A15],
        a15: TC[A16],
        a16: TC[A17]
    ): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation18P[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {
    val axo = AndXorK18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Apply[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]],
        a12: TC[F[A13]],
        a13: TC[F[A14]],
        a14: TC[F[A15]],
        a15: TC[F[A16]],
        a16: TC[F[A17]],
        a17: TC[F[A18]]
    ): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Divide[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]],
        a12: TC[F[A13]],
        a13: TC[F[A14]],
        a14: TC[F[A15]],
        a15: TC[F[A16]],
        a16: TC[F[A17]],
        a17: TC[F[A18]]
    ): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation18PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {
    val axo = AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Apply[TC],
        a0: TC[A1],
        a1: TC[A2],
        a2: TC[A3],
        a3: TC[A4],
        a4: TC[A5],
        a5: TC[A6],
        a6: TC[A7],
        a7: TC[A8],
        a8: TC[A9],
        a9: TC[A10],
        a10: TC[A11],
        a11: TC[A12],
        a12: TC[A13],
        a13: TC[A14],
        a14: TC[A15],
        a15: TC[A16],
        a16: TC[A17],
        a17: TC[A18]
    ): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Divide[TC],
        a0: TC[A1],
        a1: TC[A2],
        a2: TC[A3],
        a3: TC[A4],
        a4: TC[A5],
        a5: TC[A6],
        a6: TC[A7],
        a7: TC[A8],
        a8: TC[A9],
        a9: TC[A10],
        a10: TC[A11],
        a11: TC[A12],
        a12: TC[A13],
        a13: TC[A14],
        a14: TC[A15],
        a15: TC[A16],
        a16: TC[A17],
        a17: TC[A18]
    ): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation19P[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] {
    val axo = AndXorK19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Apply[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]],
        a12: TC[F[A13]],
        a13: TC[F[A14]],
        a14: TC[F[A15]],
        a15: TC[F[A16]],
        a16: TC[F[A17]],
        a17: TC[F[A18]],
        a18: TC[F[A19]]
    ): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Divide[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]],
        a12: TC[F[A13]],
        a13: TC[F[A14]],
        a14: TC[F[A15]],
        a15: TC[F[A16]],
        a16: TC[F[A17]],
        a17: TC[F[A18]],
        a18: TC[F[A19]]
    ): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation19PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] {
    val axo = AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Apply[TC],
        a0: TC[A1],
        a1: TC[A2],
        a2: TC[A3],
        a3: TC[A4],
        a4: TC[A5],
        a5: TC[A6],
        a6: TC[A7],
        a7: TC[A8],
        a8: TC[A9],
        a9: TC[A10],
        a10: TC[A11],
        a11: TC[A12],
        a12: TC[A13],
        a13: TC[A14],
        a14: TC[A15],
        a15: TC[A16],
        a16: TC[A17],
        a17: TC[A18],
        a18: TC[A19]
    ): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Divide[TC],
        a0: TC[A1],
        a1: TC[A2],
        a2: TC[A3],
        a3: TC[A4],
        a4: TC[A5],
        a5: TC[A6],
        a6: TC[A7],
        a7: TC[A8],
        a8: TC[A9],
        a9: TC[A10],
        a10: TC[A11],
        a11: TC[A12],
        a12: TC[A13],
        a13: TC[A14],
        a14: TC[A15],
        a15: TC[A16],
        a16: TC[A17],
        a17: TC[A18],
        a18: TC[A19]
    ): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation20P[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {
    val axo = AndXorK20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Apply[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]],
        a12: TC[F[A13]],
        a13: TC[F[A14]],
        a14: TC[F[A15]],
        a15: TC[F[A16]],
        a16: TC[F[A17]],
        a17: TC[F[A18]],
        a18: TC[F[A19]],
        a19: TC[F[A20]]
    ): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Divide[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]],
        a12: TC[F[A13]],
        a13: TC[F[A14]],
        a14: TC[F[A15]],
        a15: TC[F[A16]],
        a16: TC[F[A17]],
        a17: TC[F[A18]],
        a18: TC[F[A19]],
        a19: TC[F[A20]]
    ): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation20PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {
    val axo = AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Apply[TC],
        a0: TC[A1],
        a1: TC[A2],
        a2: TC[A3],
        a3: TC[A4],
        a4: TC[A5],
        a5: TC[A6],
        a6: TC[A7],
        a7: TC[A8],
        a8: TC[A9],
        a9: TC[A10],
        a10: TC[A11],
        a11: TC[A12],
        a12: TC[A13],
        a13: TC[A14],
        a14: TC[A15],
        a15: TC[A16],
        a16: TC[A17],
        a17: TC[A18],
        a18: TC[A19],
        a19: TC[A20]
    ): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Divide[TC],
        a0: TC[A1],
        a1: TC[A2],
        a2: TC[A3],
        a3: TC[A4],
        a4: TC[A5],
        a5: TC[A6],
        a6: TC[A7],
        a7: TC[A8],
        a8: TC[A9],
        a9: TC[A10],
        a10: TC[A11],
        a11: TC[A12],
        a12: TC[A13],
        a13: TC[A14],
        a14: TC[A15],
        a15: TC[A16],
        a16: TC[A17],
        a17: TC[A18],
        a18: TC[A19],
        a19: TC[A20]
    ): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation21P[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {
    val axo = AndXorK21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Apply[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]],
        a12: TC[F[A13]],
        a13: TC[F[A14]],
        a14: TC[F[A15]],
        a15: TC[F[A16]],
        a16: TC[F[A17]],
        a17: TC[F[A18]],
        a18: TC[F[A19]],
        a19: TC[F[A20]],
        a20: TC[F[A21]]
    ): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Divide[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]],
        a12: TC[F[A13]],
        a13: TC[F[A14]],
        a14: TC[F[A15]],
        a15: TC[F[A16]],
        a16: TC[F[A17]],
        a17: TC[F[A18]],
        a18: TC[F[A19]],
        a19: TC[F[A20]],
        a20: TC[F[A21]]
    ): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation21PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {
    val axo = AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Apply[TC],
        a0: TC[A1],
        a1: TC[A2],
        a2: TC[A3],
        a3: TC[A4],
        a4: TC[A5],
        a5: TC[A6],
        a6: TC[A7],
        a7: TC[A8],
        a8: TC[A9],
        a9: TC[A10],
        a10: TC[A11],
        a11: TC[A12],
        a12: TC[A13],
        a13: TC[A14],
        a14: TC[A15],
        a15: TC[A16],
        a16: TC[A17],
        a17: TC[A18],
        a18: TC[A19],
        a19: TC[A20],
        a20: TC[A21]
    ): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Divide[TC],
        a0: TC[A1],
        a1: TC[A2],
        a2: TC[A3],
        a3: TC[A4],
        a4: TC[A5],
        a5: TC[A6],
        a6: TC[A7],
        a7: TC[A8],
        a8: TC[A9],
        a9: TC[A10],
        a10: TC[A11],
        a11: TC[A12],
        a12: TC[A13],
        a13: TC[A14],
        a14: TC[A15],
        a15: TC[A16],
        a16: TC[A17],
        a17: TC[A18],
        a18: TC[A19],
        a19: TC[A20],
        a20: TC[A21]
    ): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation22P[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {
    val axo = AndXorK22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Apply[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]],
        a12: TC[F[A13]],
        a13: TC[F[A14]],
        a14: TC[F[A15]],
        a15: TC[F[A16]],
        a16: TC[F[A17]],
        a17: TC[F[A18]],
        a18: TC[F[A19]],
        a19: TC[F[A20]],
        a20: TC[F[A21]],
        a21: TC[F[A22]]
    ): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Divide[TC],
        a0: TC[F[A1]],
        a1: TC[F[A2]],
        a2: TC[F[A3]],
        a3: TC[F[A4]],
        a4: TC[F[A5]],
        a5: TC[F[A6]],
        a6: TC[F[A7]],
        a7: TC[F[A8]],
        a8: TC[F[A9]],
        a9: TC[F[A10]],
        a10: TC[F[A11]],
        a11: TC[F[A12]],
        a12: TC[F[A13]],
        a13: TC[F[A14]],
        a14: TC[F[A15]],
        a15: TC[F[A16]],
        a16: TC[F[A17]],
        a17: TC[F[A18]],
        a18: TC[F[A19]],
        a19: TC[F[A20]],
        a20: TC[F[A21]],
        a21: TC[F[A22]]
    ): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait Derivation22PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {
    val axo = AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    def deriveCovariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Apply[TC],
        a0: TC[A1],
        a1: TC[A2],
        a2: TC[A3],
        a3: TC[A4],
        a4: TC[A5],
        a5: TC[A6],
        a6: TC[A7],
        a7: TC[A8],
        a8: TC[A9],
        a9: TC[A10],
        a10: TC[A11],
        a11: TC[A12],
        a12: TC[A13],
        a13: TC[A14],
        a14: TC[A15],
        a15: TC[A16],
        a16: TC[A17],
        a17: TC[A18],
        a18: TC[A19],
        a19: TC[A20],
        a20: TC[A21],
        a21: TC[A22]
    ): TC[A] =
      F.map(axo.combine[TC].apply)(iso.from(_))

    def deriveContravariant[TC[_], A](iso: A <=> axo.Prod)(
        implicit F: Divide[TC],
        a0: TC[A1],
        a1: TC[A2],
        a2: TC[A3],
        a3: TC[A4],
        a4: TC[A5],
        a5: TC[A6],
        a6: TC[A7],
        a7: TC[A8],
        a8: TC[A9],
        a9: TC[A10],
        a10: TC[A11],
        a11: TC[A12],
        a12: TC[A13],
        a13: TC[A14],
        a14: TC[A15],
        a15: TC[A16],
        a16: TC[A17],
        a17: TC[A18],
        a18: TC[A19],
        a19: TC[A20],
        a20: TC[A21],
        a21: TC[A22]
    ): TC[A] =
      F.contramap(axo.combine[TC].divide)(iso.to(_))
  }

  trait LPSyntax {

    implicit class Derivation1POps[A, F[_], A1](iso: A <=> AndXorK1[F, A1]#Prod) {
      val derive = new Derivation1P[F, A1] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], a0: TC[F[A1]]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], a0: TC[F[A1]]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation2POps[A, F[_], A1, A2](iso: A <=> AndXorK2[F, A1, A2]#Prod) {
      val derive = new Derivation2P[F, A1, A2] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], a0: TC[F[A1]], a1: TC[F[A2]]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], a0: TC[F[A1]], a1: TC[F[A2]]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation3POps[A, F[_], A1, A2, A3](iso: A <=> AndXorK3[F, A1, A2, A3]#Prod) {
      val derive = new Derivation3P[F, A1, A2, A3] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation4POps[A, F[_], A1, A2, A3, A4](iso: A <=> AndXorK4[F, A1, A2, A3, A4]#Prod) {
      val derive = new Derivation4P[F, A1, A2, A3, A4] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation5POps[A, F[_], A1, A2, A3, A4, A5](iso: A <=> AndXorK5[F, A1, A2, A3, A4, A5]#Prod) {
      val derive = new Derivation5P[F, A1, A2, A3, A4, A5] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation6POps[A, F[_], A1, A2, A3, A4, A5, A6](iso: A <=> AndXorK6[F, A1, A2, A3, A4, A5, A6]#Prod) {
      val derive = new Derivation6P[F, A1, A2, A3, A4, A5, A6] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]], a5: TC[F[A6]]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]], a5: TC[F[A6]]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation7POps[A, F[_], A1, A2, A3, A4, A5, A6, A7](iso: A <=> AndXorK7[F, A1, A2, A3, A4, A5, A6, A7]#Prod) {
      val derive = new Derivation7P[F, A1, A2, A3, A4, A5, A6, A7] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]], a5: TC[F[A6]], a6: TC[F[A7]]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]], a5: TC[F[A6]], a6: TC[F[A7]]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation8POps[A, F[_], A1, A2, A3, A4, A5, A6, A7, A8](iso: A <=> AndXorK8[F, A1, A2, A3, A4, A5, A6, A7, A8]#Prod) {
      val derive = new Derivation8P[F, A1, A2, A3, A4, A5, A6, A7, A8] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]], a5: TC[F[A6]], a6: TC[F[A7]], a7: TC[F[A8]]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]], a5: TC[F[A6]], a6: TC[F[A7]], a7: TC[F[A8]]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation9POps[A, F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](iso: A <=> AndXorK9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]#Prod) {
      val derive = new Derivation9P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]], a5: TC[F[A6]], a6: TC[F[A7]], a7: TC[F[A8]], a8: TC[F[A9]]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], a0: TC[F[A1]], a1: TC[F[A2]], a2: TC[F[A3]], a3: TC[F[A4]], a4: TC[F[A5]], a5: TC[F[A6]], a6: TC[F[A7]], a7: TC[F[A8]], a8: TC[F[A9]])
          : TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation10POps[A, F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](iso: A <=> AndXorK10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]#Prod) {
      val derive = new Derivation10P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation11POps[A, F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](iso: A <=> AndXorK11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]#Prod) {
      val derive = new Derivation11P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation12POps[A, F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](iso: A <=> AndXorK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod) {
      val derive = new Derivation12P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation13POps[A, F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](iso: A <=> AndXorK13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]#Prod) {
      val derive = new Derivation13P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]],
          a12: TC[F[A13]]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]],
          a12: TC[F[A13]]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation14POps[A, F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](iso: A <=> AndXorK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Prod) {
      val derive = new Derivation14P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]],
          a12: TC[F[A13]],
          a13: TC[F[A14]]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]],
          a12: TC[F[A13]],
          a13: TC[F[A14]]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation15POps[A, F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        iso: A <=> AndXorK15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]#Prod
    ) {
      val derive = new Derivation15P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]],
          a12: TC[F[A13]],
          a13: TC[F[A14]],
          a14: TC[F[A15]]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]],
          a12: TC[F[A13]],
          a13: TC[F[A14]],
          a14: TC[F[A15]]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation16POps[A, F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        iso: A <=> AndXorK16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]#Prod
    ) {
      val derive = new Derivation16P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]],
          a12: TC[F[A13]],
          a13: TC[F[A14]],
          a14: TC[F[A15]],
          a15: TC[F[A16]]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]],
          a12: TC[F[A13]],
          a13: TC[F[A14]],
          a14: TC[F[A15]],
          a15: TC[F[A16]]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation17POps[A, F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        iso: A <=> AndXorK17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]#Prod
    ) {
      val derive = new Derivation17P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]],
          a12: TC[F[A13]],
          a13: TC[F[A14]],
          a14: TC[F[A15]],
          a15: TC[F[A16]],
          a16: TC[F[A17]]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]],
          a12: TC[F[A13]],
          a13: TC[F[A14]],
          a14: TC[F[A15]],
          a15: TC[F[A16]],
          a16: TC[F[A17]]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation18POps[A, F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        iso: A <=> AndXorK18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]#Prod
    ) {
      val derive = new Derivation18P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]],
          a12: TC[F[A13]],
          a13: TC[F[A14]],
          a14: TC[F[A15]],
          a15: TC[F[A16]],
          a16: TC[F[A17]],
          a17: TC[F[A18]]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]],
          a12: TC[F[A13]],
          a13: TC[F[A14]],
          a14: TC[F[A15]],
          a15: TC[F[A16]],
          a16: TC[F[A17]],
          a17: TC[F[A18]]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation19POps[A, F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        iso: A <=> AndXorK19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]#Prod
    ) {
      val derive = new Derivation19P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]],
          a12: TC[F[A13]],
          a13: TC[F[A14]],
          a14: TC[F[A15]],
          a15: TC[F[A16]],
          a16: TC[F[A17]],
          a17: TC[F[A18]],
          a18: TC[F[A19]]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]],
          a12: TC[F[A13]],
          a13: TC[F[A14]],
          a14: TC[F[A15]],
          a15: TC[F[A16]],
          a16: TC[F[A17]],
          a17: TC[F[A18]],
          a18: TC[F[A19]]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation20POps[A, F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        iso: A <=> AndXorK20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]#Prod
    ) {
      val derive = new Derivation20P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]],
          a12: TC[F[A13]],
          a13: TC[F[A14]],
          a14: TC[F[A15]],
          a15: TC[F[A16]],
          a16: TC[F[A17]],
          a17: TC[F[A18]],
          a18: TC[F[A19]],
          a19: TC[F[A20]]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]],
          a12: TC[F[A13]],
          a13: TC[F[A14]],
          a14: TC[F[A15]],
          a15: TC[F[A16]],
          a16: TC[F[A17]],
          a17: TC[F[A18]],
          a18: TC[F[A19]],
          a19: TC[F[A20]]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation21POps[A, F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        iso: A <=> AndXorK21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]#Prod
    ) {
      val derive = new Derivation21P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]],
          a12: TC[F[A13]],
          a13: TC[F[A14]],
          a14: TC[F[A15]],
          a15: TC[F[A16]],
          a16: TC[F[A17]],
          a17: TC[F[A18]],
          a18: TC[F[A19]],
          a19: TC[F[A20]],
          a20: TC[F[A21]]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]],
          a12: TC[F[A13]],
          a13: TC[F[A14]],
          a14: TC[F[A15]],
          a15: TC[F[A16]],
          a16: TC[F[A17]],
          a17: TC[F[A18]],
          a18: TC[F[A19]],
          a19: TC[F[A20]],
          a20: TC[F[A21]]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation22POps[A, F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        iso: A <=> AndXorK22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]#Prod
    ) {
      val derive = new Derivation22P[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]],
          a12: TC[F[A13]],
          a13: TC[F[A14]],
          a14: TC[F[A15]],
          a15: TC[F[A16]],
          a16: TC[F[A17]],
          a17: TC[F[A18]],
          a18: TC[F[A19]],
          a19: TC[F[A20]],
          a20: TC[F[A21]],
          a21: TC[F[A22]]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[F[A1]],
          a1: TC[F[A2]],
          a2: TC[F[A3]],
          a3: TC[F[A4]],
          a4: TC[F[A5]],
          a5: TC[F[A6]],
          a6: TC[F[A7]],
          a7: TC[F[A8]],
          a8: TC[F[A9]],
          a9: TC[F[A10]],
          a10: TC[F[A11]],
          a11: TC[F[A12]],
          a12: TC[F[A13]],
          a13: TC[F[A14]],
          a14: TC[F[A15]],
          a15: TC[F[A16]],
          a16: TC[F[A17]],
          a17: TC[F[A18]],
          a18: TC[F[A19]],
          a19: TC[F[A20]],
          a20: TC[F[A21]],
          a21: TC[F[A22]]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

  }

  object syntax extends LPSyntax {

    implicit class Derivation1POps[A, A1](iso: A <=> AndXor1[A1]#Prod) {
      val derive = new Derivation1PId[A1] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], a0: TC[A1]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], a0: TC[A1]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation2POps[A, A1, A2](iso: A <=> AndXor2[A1, A2]#Prod) {
      val derive = new Derivation2PId[A1, A2] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], a0: TC[A1], a1: TC[A2]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], a0: TC[A1], a1: TC[A2]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation3POps[A, A1, A2, A3](iso: A <=> AndXor3[A1, A2, A3]#Prod) {
      val derive = new Derivation3PId[A1, A2, A3] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation4POps[A, A1, A2, A3, A4](iso: A <=> AndXor4[A1, A2, A3, A4]#Prod) {
      val derive = new Derivation4PId[A1, A2, A3, A4] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation5POps[A, A1, A2, A3, A4, A5](iso: A <=> AndXor5[A1, A2, A3, A4, A5]#Prod) {
      val derive = new Derivation5PId[A1, A2, A3, A4, A5] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation6POps[A, A1, A2, A3, A4, A5, A6](iso: A <=> AndXor6[A1, A2, A3, A4, A5, A6]#Prod) {
      val derive = new Derivation6PId[A1, A2, A3, A4, A5, A6] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation7POps[A, A1, A2, A3, A4, A5, A6, A7](iso: A <=> AndXor7[A1, A2, A3, A4, A5, A6, A7]#Prod) {
      val derive = new Derivation7PId[A1, A2, A3, A4, A5, A6, A7] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation8POps[A, A1, A2, A3, A4, A5, A6, A7, A8](iso: A <=> AndXor8[A1, A2, A3, A4, A5, A6, A7, A8]#Prod) {
      val derive = new Derivation8PId[A1, A2, A3, A4, A5, A6, A7, A8] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7], a7: TC[A8]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7], a7: TC[A8]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation9POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9](iso: A <=> AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9]#Prod) {
      val derive = new Derivation9PId[A1, A2, A3, A4, A5, A6, A7, A8, A9] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7], a7: TC[A8], a8: TC[A9]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7], a7: TC[A8], a8: TC[A9]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation10POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](iso: A <=> AndXor10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]#Prod) {
      val derive = new Derivation10PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7], a7: TC[A8], a8: TC[A9], a9: TC[A10]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7], a7: TC[A8], a8: TC[A9], a9: TC[A10]): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation11POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](iso: A <=> AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]#Prod) {
      val derive = new Derivation11PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7], a7: TC[A8], a8: TC[A9], a9: TC[A10], a10: TC[A11]): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], a0: TC[A1], a1: TC[A2], a2: TC[A3], a3: TC[A4], a4: TC[A5], a5: TC[A6], a6: TC[A7], a7: TC[A8], a8: TC[A9], a9: TC[A10], a10: TC[A11])
          : TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation12POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](iso: A <=> AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod) {
      val derive = new Derivation12PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation13POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](iso: A <=> AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]#Prod) {
      val derive = new Derivation13PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12],
          a12: TC[A13]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12],
          a12: TC[A13]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation14POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](iso: A <=> AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Prod) {
      val derive = new Derivation14PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12],
          a12: TC[A13],
          a13: TC[A14]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12],
          a12: TC[A13],
          a13: TC[A14]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation15POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](iso: A <=> AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]#Prod) {
      val derive = new Derivation15PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12],
          a12: TC[A13],
          a13: TC[A14],
          a14: TC[A15]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12],
          a12: TC[A13],
          a13: TC[A14],
          a14: TC[A15]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation16POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        iso: A <=> AndXor16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]#Prod
    ) {
      val derive = new Derivation16PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12],
          a12: TC[A13],
          a13: TC[A14],
          a14: TC[A15],
          a15: TC[A16]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12],
          a12: TC[A13],
          a13: TC[A14],
          a14: TC[A15],
          a15: TC[A16]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation17POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        iso: A <=> AndXor17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]#Prod
    ) {
      val derive = new Derivation17PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12],
          a12: TC[A13],
          a13: TC[A14],
          a14: TC[A15],
          a15: TC[A16],
          a16: TC[A17]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12],
          a12: TC[A13],
          a13: TC[A14],
          a14: TC[A15],
          a15: TC[A16],
          a16: TC[A17]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation18POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        iso: A <=> AndXor18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]#Prod
    ) {
      val derive = new Derivation18PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12],
          a12: TC[A13],
          a13: TC[A14],
          a14: TC[A15],
          a15: TC[A16],
          a16: TC[A17],
          a17: TC[A18]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12],
          a12: TC[A13],
          a13: TC[A14],
          a14: TC[A15],
          a15: TC[A16],
          a16: TC[A17],
          a17: TC[A18]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation19POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        iso: A <=> AndXor19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]#Prod
    ) {
      val derive = new Derivation19PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12],
          a12: TC[A13],
          a13: TC[A14],
          a14: TC[A15],
          a15: TC[A16],
          a16: TC[A17],
          a17: TC[A18],
          a18: TC[A19]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12],
          a12: TC[A13],
          a13: TC[A14],
          a14: TC[A15],
          a15: TC[A16],
          a16: TC[A17],
          a17: TC[A18],
          a18: TC[A19]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation20POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        iso: A <=> AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]#Prod
    ) {
      val derive = new Derivation20PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12],
          a12: TC[A13],
          a13: TC[A14],
          a14: TC[A15],
          a15: TC[A16],
          a16: TC[A17],
          a17: TC[A18],
          a18: TC[A19],
          a19: TC[A20]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12],
          a12: TC[A13],
          a13: TC[A14],
          a14: TC[A15],
          a15: TC[A16],
          a16: TC[A17],
          a17: TC[A18],
          a18: TC[A19],
          a19: TC[A20]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation21POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        iso: A <=> AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]#Prod
    ) {
      val derive = new Derivation21PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12],
          a12: TC[A13],
          a13: TC[A14],
          a14: TC[A15],
          a15: TC[A16],
          a16: TC[A17],
          a17: TC[A18],
          a18: TC[A19],
          a19: TC[A20],
          a20: TC[A21]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12],
          a12: TC[A13],
          a13: TC[A14],
          a14: TC[A15],
          a15: TC[A16],
          a16: TC[A17],
          a17: TC[A18],
          a18: TC[A19],
          a19: TC[A20],
          a20: TC[A21]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

    implicit class Derivation22POps[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        iso: A <=> AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]#Prod
    ) {
      val derive = new Derivation22PId[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {}

      def deriveCovariant[TC[_]](
          implicit F: Apply[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12],
          a12: TC[A13],
          a13: TC[A14],
          a14: TC[A15],
          a15: TC[A16],
          a16: TC[A17],
          a17: TC[A18],
          a18: TC[A19],
          a19: TC[A20],
          a20: TC[A21],
          a21: TC[A22]
      ): TC[A] =
        derive.deriveCovariant[TC, A](iso)

      def deriveContravariant[TC[_]](
          implicit F: Divide[TC],
          a0: TC[A1],
          a1: TC[A2],
          a2: TC[A3],
          a3: TC[A4],
          a4: TC[A5],
          a5: TC[A6],
          a6: TC[A7],
          a7: TC[A8],
          a8: TC[A9],
          a9: TC[A10],
          a10: TC[A11],
          a11: TC[A12],
          a12: TC[A13],
          a13: TC[A14],
          a14: TC[A15],
          a15: TC[A16],
          a16: TC[A17],
          a17: TC[A18],
          a18: TC[A19],
          a19: TC[A20],
          a20: TC[A21],
          a21: TC[A22]
      ): TC[A] =
        derive.deriveContravariant[TC, A](iso)
    }

  }
}
