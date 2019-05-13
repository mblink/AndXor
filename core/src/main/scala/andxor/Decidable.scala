package andxor

import scalaz.{\/, Contravariant}

trait Decidable[F[_]] extends Contravariant[F] {
  def choose2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(f: Z => (A1 \/ A2)): F[Z]
}

object Decidable {
  implicit def decidableFunction1[O]: Decidable[? => O] = new Decidable[? => O] {
    def contramap[A, B](fa: A => O)(f: B => A): B => O = b => fa(f(b))
    def choose2[Z, A1, A2](a1: => A1 => O, a2: => A2 => O)(f: Z => (A1 \/ A2)): Z => O = f(_).fold(a1(_), a2(_))
  }
}
