package andxor

import java.lang.Void
import scalaz.{\/, Contravariant}

trait Decidable[F[_]] extends Contravariant[F] {
  def absurd[A]: Void => A = _ => sys.error("unpossible")

  def lose[A](f: A => Void): F[A]

  def choose2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(f: Z => (A1 \/ A2)): F[Z]

  def contramap[A, B](fa: F[A])(f: B => A): F[B] = choose2[B, Void, A](lose(identity), fa)(b => \/.right(f(b)))
}
