package andxor

import scalaz.{Contravariant, Equal, IsomorphismContravariant, Kleisli, Show}
import scalaz.Isomorphism.<~>

trait Decidable[F[_]] extends Contravariant[F] {
  def choose2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(f: Z => Either[A1, A2]): F[Z]
}

object Decidable {
  def apply[F[_]](implicit ev: Decidable[F]): Decidable[F] = ev

  def fromIso[F[_], G[_]](i: F <~> G)(implicit D: Decidable[G]): Decidable[F] =
    new Decidable[F] with IsomorphismContravariant[F, G] {
      implicit val G: Contravariant[G] = D
      val iso: F <~> G = i

      def choose2[Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: Z => Either[A1, A2]): F[Z] =
        iso.from(D.choose2(iso.to(a1), iso.to(a2))(f))
    }

  implicit def decidableFunction1[O]: Decidable[? => O] = new Decidable[? => O] {
    def contramap[A, B](fa: A => O)(f: B => A): B => O = b => fa(f(b))
    def choose2[Z, A1, A2](a1: => A1 => O, a2: => A2 => O)(f: Z => Either[A1, A2]): Z => O = f(_).fold(a1(_), a2(_))
  }

  implicit def decidableKleisli[F[_], O]: Decidable[Kleisli[F, ?, O]] = new Decidable[Kleisli[F, ?, O]] {
    def contramap[A, B](fa: Kleisli[F, A, O])(f: B => A): Kleisli[F, B, O] = Kleisli(b => fa.run(f(b)))
    def choose2[Z, A1, A2](a1: => Kleisli[F, A1, O], a2: => Kleisli[F, A2, O])(f: Z => Either[A1, A2]): Kleisli[F, Z, O] =
      Kleisli(f(_).fold(a1.run(_), a2.run(_)))
  }

  implicit val decideEqual: Decidable[Equal] = new Decidable[Equal] {
    def contramap[A, B](fa: Equal[A])(f: B => A): Equal[B] = fa.contramap(f)
    def choose2[Z, A1, A2](a1: => Equal[A1], a2: => Equal[A2])(f: Z => Either[A1, A2]): Equal[Z] =
      Equal.equal((z1, z2) => (f(z1), f(z2)) match {
        case (Left(s1), Left(s2)) => a1.equal(s1, s2)
        case (Right(t1), Right(t2)) => a2.equal(t1, t2)
        case (_, _) => false
      })
  }

  implicit val decideShow: Decidable[Show] = new Decidable[Show] {
    def contramap[A, B](fa: Show[A])(f: B => A): Show[B] = Show.show(b => fa.show(f(b)))
    def choose2[Z, A1, A2](a1: => Show[A1], a2: => Show[A2])(f: Z => Either[A1, A2]): Show[Z] =
      Show.show(f(_).fold(a1.show(_), a2.show(_)))
  }
}
