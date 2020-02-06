package andxor

import scalaz.Isomorphism.<=>

object derivation {

  trait Derivation2C[A1, A2] {
    val axo = AndXor2[A1, A2]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation2P[A1, A2] {
    val axo = AndXor2[A1, A2]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation3C[A1, A2, A3] {
    val axo = AndXor3[A1, A2, A3]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation3P[A1, A2, A3] {
    val axo = AndXor3[A1, A2, A3]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation4C[A1, A2, A3, A4] {
    val axo = AndXor4[A1, A2, A3, A4]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation4P[A1, A2, A3, A4] {
    val axo = AndXor4[A1, A2, A3, A4]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation5C[A1, A2, A3, A4, A5] {
    val axo = AndXor5[A1, A2, A3, A4, A5]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation5P[A1, A2, A3, A4, A5] {
    val axo = AndXor5[A1, A2, A3, A4, A5]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  trait Derivation6C[A1, A2, A3, A4, A5, A6] {
    val axo = AndXor6[A1, A2, A3, A4, A5, A6]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]]): TC[A] =
      F.map(axo.deriving[TC, F].alt)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Cop[F])(implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]]): TC[A] =
      F.contramap(axo.deriving[TC, F].choose)(iso.to(_))
  }

  trait Derivation6P[A1, A2, A3, A4, A5, A6] {
    val axo = AndXor6[A1, A2, A3, A4, A5, A6]

    def deriveCovariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]]): TC[A] =
      F.map(axo.deriving[TC, F].apply)(iso.from(_))

    def deriveContravariant[TC[_], F[_], A](iso: A <=> axo.Prod[F])(implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]]): TC[A] =
      F.contramap(axo.deriving[TC, F].divide)(iso.to(_))
  }

  object ops {

    implicit class Derivation2COps[A, A1, A2, F[_]](iso: A <=> AndXor2[A1, A2]#Cop[F]) {
      val derive = new Derivation2C[A1, A2] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation2POps[A, A1, A2, F[_]](iso: A <=> AndXor2[A1, A2]#Prod[F]) {
      val derive = new Derivation2P[A1, A2] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation3COps[A, A1, A2, A3, F[_]](iso: A <=> AndXor3[A1, A2, A3]#Cop[F]) {
      val derive = new Derivation3C[A1, A2, A3] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation3POps[A, A1, A2, A3, F[_]](iso: A <=> AndXor3[A1, A2, A3]#Prod[F]) {
      val derive = new Derivation3P[A1, A2, A3] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation4COps[A, A1, A2, A3, A4, F[_]](iso: A <=> AndXor4[A1, A2, A3, A4]#Cop[F]) {
      val derive = new Derivation4C[A1, A2, A3, A4] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation4POps[A, A1, A2, A3, A4, F[_]](iso: A <=> AndXor4[A1, A2, A3, A4]#Prod[F]) {
      val derive = new Derivation4P[A1, A2, A3, A4] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation5COps[A, A1, A2, A3, A4, A5, F[_]](iso: A <=> AndXor5[A1, A2, A3, A4, A5]#Cop[F]) {
      val derive = new Derivation5C[A1, A2, A3, A4, A5] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation5POps[A, A1, A2, A3, A4, A5, F[_]](iso: A <=> AndXor5[A1, A2, A3, A4, A5]#Prod[F]) {
      val derive = new Derivation5P[A1, A2, A3, A4, A5] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation6COps[A, A1, A2, A3, A4, A5, A6, F[_]](iso: A <=> AndXor6[A1, A2, A3, A4, A5, A6]#Cop[F]) {
      val derive = new Derivation6C[A1, A2, A3, A4, A5, A6] {}

      def deriveCovariant[TC[_]](implicit F: Alt[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Decidable[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

    implicit class Derivation6POps[A, A1, A2, A3, A4, A5, A6, F[_]](iso: A <=> AndXor6[A1, A2, A3, A4, A5, A6]#Prod[F]) {
      val derive = new Derivation6P[A1, A2, A3, A4, A5, A6] {}

      def deriveCovariant[TC[_]](implicit F: Apply[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]]): TC[A] =
        derive.deriveCovariant[TC, F, A](iso)

      def deriveContravariant[TC[_]](implicit F: Divide[TC], t0: TC[F[A1]], t1: TC[F[A2]], t2: TC[F[A3]], t3: TC[F[A4]], t4: TC[F[A5]], t5: TC[F[A6]]): TC[A] =
        derive.deriveContravariant[TC, F, A](iso)
    }

  }
}
