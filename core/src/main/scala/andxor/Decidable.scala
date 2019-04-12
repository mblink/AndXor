package andxor

import scalaz.{\/, Contravariant}

trait Decidable[F[_]] extends Contravariant[F] {
  def choose2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(f: Z => (A1 \/ A2)): F[Z]
}

