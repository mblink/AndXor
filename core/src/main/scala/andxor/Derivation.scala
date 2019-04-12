package andxor

import scalaz.{Contravariant, Functor}
import scalaz.Isomorphism.<=>

object derivation { obj =>
  def deriveCovariant[A, Repr, TC[_]](iso: A <=> Repr)(implicit F: Functor[TC], tc: TC[Repr]): TC[A] =
    F.map[Repr, A](tc)(iso.from(_))

  def deriveContravariant[A, Repr, TC[_]](iso: A <=> Repr)(implicit F: Contravariant[TC], tc: TC[Repr]): TC[A] =
    F.contramap[Repr, A](tc)(iso.to(_))

  object syntax {
    implicit class DerivationOps[A, Repr](iso: A <=> Repr) {
      def deriveCovariant[TC[_]](implicit F: Functor[TC], tc: TC[Repr]): TC[A] = obj.deriveCovariant(iso)
      def deriveContravariant[TC[_]](implicit F: Contravariant[TC], tc: TC[Repr]): TC[A] = obj.deriveContravariant(iso)
    }
  }
}
