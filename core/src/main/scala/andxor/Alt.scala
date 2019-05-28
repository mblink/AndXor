package andxor

import scala.language.higherKinds
import scalaz.{\/, Applicative, IsomorphismApplicative}
import scalaz.Isomorphism.<~>
import scalaz.syntax.either._

trait Alt[F[_]] extends Applicative[F] {
  def alt[A](a1: =>F[A], a2: =>F[A]): F[A]
  def altly2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(f: A1 \/ A2 => Z): F[Z] =
    map(alt(map(a1)(_.left[A2]), map(a2)(_.right[A1])))(f)
}

object Alt {
  def fromIso[F[_], G[_]](i: F <~> G)(implicit E: Alt[G]): Alt[F] =
    new Alt[F] with IsomorphismApplicative[F, G] {
      implicit val G: Alt[G] = E
      val iso: F <~> G = i

      def alt[A](a1: => F[A], a2: => F[A]): F[A] = iso.from(G.alt(iso.to(a1), iso.to(a2)))
    }
}
