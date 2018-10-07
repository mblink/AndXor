package andxor

import scala.language.higherKinds
import scalaz.\/

trait Decidable[F[_]] {
  def choose2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(f: Z => (A1 \/ A2)): F[Z]
}

