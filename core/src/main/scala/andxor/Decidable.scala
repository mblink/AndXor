package andxor

import scalaz.{\/, Contravariant, IsomorphismContravariant}
import scalaz.Isomorphism.<~>

trait Decidable[F[_]] extends Contravariant[F] {
  def choose2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(f: Z => (A1 \/ A2)): F[Z]
}

object Decidable {
  def apply[F[_]](implicit ev: Decidable[F]): Decidable[F] = ev

  def fromIso[F[_], G[_]](i: F <~> G)(implicit D: Decidable[G]): Decidable[F] =
    new Decidable[F] with IsomorphismContravariant[F, G] {
      implicit val G: Contravariant[G] = D
      val iso: F <~> G = i

      def choose2[Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: Z => (A1 \/ A2)): F[Z] =
        iso.from(D.choose2(iso.to(a1), iso.to(a2))(f))
    }

  implicit def decidableFunction1[O]: Decidable[? => O] = new Decidable[? => O] {
    def contramap[A, B](fa: A => O)(f: B => A): B => O = b => fa(f(b))
    def choose2[Z, A1, A2](a1: => A1 => O, a2: => A2 => O)(f: Z => (A1 \/ A2)): Z => O = f(_).fold(a1(_), a2(_))
  }
}
