package andxor

import cats.{~>, Contravariant, Eq, Show}
import cats.data.Kleisli

trait Decidable[F[_]] extends Contravariant[F] {
  def choose2[Z, A1, A2](a1: F[A1], a2: F[A2])(f: Z => Either[A1, A2]): F[Z]
}

object Decidable {
  @inline def apply[F[_]](implicit ev: Decidable[F]): Decidable[F] = ev

  def fromIso[F[_], G[_]](fg: F ~> G, gf: G ~> F)(implicit G: Decidable[G]): Decidable[F] =
    new Decidable[F] {
      def contramap[A, B](fa: F[A])(f: B => A): F[B] = gf(G.contramap(fg(fa))(f))
      def choose2[Z, A1, A2](a1: F[A1], a2: F[A2])(f: Z => Either[A1, A2]): F[Z] =
        gf(G.choose2(fg(a1), fg(a2))(f))
    }

  implicit def decidableFunction1[O]: Decidable[* => O] = new Decidable[* => O] {
    def contramap[A, B](fa: A => O)(f: B => A): B => O = b => fa(f(b))
    def choose2[Z, A1, A2](a1: A1 => O, a2: A2 => O)(f: Z => Either[A1, A2]): Z => O = f(_).fold(a1(_), a2(_))
  }

  implicit def decidableKleisli[F[_], O]: Decidable[Kleisli[F, *, O]] = new Decidable[Kleisli[F, *, O]] {
    def contramap[A, B](fa: Kleisli[F, A, O])(f: B => A): Kleisli[F, B, O] = Kleisli(b => fa.run(f(b)))
    def choose2[Z, A1, A2](a1: Kleisli[F, A1, O], a2: Kleisli[F, A2, O])(f: Z => Either[A1, A2]): Kleisli[F, Z, O] =
      Kleisli(f(_).fold(a1.run(_), a2.run(_)))
  }

  implicit val decideEq: Decidable[Eq] = new Decidable[Eq] {
    def contramap[A, B](fa: Eq[A])(f: B => A): Eq[B] = Eq.by(f)(fa)
    def choose2[Z, A1, A2](a1: Eq[A1], a2: Eq[A2])(f: Z => Either[A1, A2]): Eq[Z] =
      Eq.instance((z1, z2) => (f(z1), f(z2)) match {
        case (Left(s1), Left(s2)) => a1.eqv(s1, s2)
        case (Right(t1), Right(t2)) => a2.eqv(t1, t2)
        case (_, _) => false
      })
  }

  implicit val decideShow: Decidable[Show] = new Decidable[Show] {
    def contramap[A, B](fa: Show[A])(f: B => A): Show[B] = Show.show(b => fa.show(f(b)))
    def choose2[Z, A1, A2](a1: Show[A1], a2: Show[A2])(f: Z => Either[A1, A2]): Show[Z] =
      Show.show(f(_).fold(a1.show(_), a2.show(_)))
  }
}
