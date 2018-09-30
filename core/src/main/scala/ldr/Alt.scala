package ldr

import scala.language.higherKinds
import scalaz.{Applicative, \/}
import scalaz.syntax.either._

trait Alt[F[_]] extends Applicative[F] {
  def alt[A](a1: =>F[A], a2: =>F[A]): F[A]
  def altly2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(f: A1 \/ A2 => Z): F[Z] =
    map(alt(map(a1)(_.left[A2]), map(a2)(_.right[A1])))(f)
}
