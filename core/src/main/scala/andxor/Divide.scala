package andxor

import scala.language.higherKinds
import scalaz.{Contravariant, IsomorphismContravariant, Semigroup}
import scalaz.Isomorphism.<~>

trait Divide[F[_]] extends Contravariant[F] {
  def conquer[A]: F[A]

  def contramap[A, B](fa: F[A])(f: B => A): F[B] = divide(conquer[Unit], fa)(b => ((), f(b)))

  final def divide[A, B, C](fa: =>F[A], fb: =>F[B])(f: C => (A, B)): F[C] =
    divide2(fa, fb)(f)

  final def divide1[A1, Z](a1: F[A1])(f: Z => A1): F[Z] = contramap(a1)(f)

  def tuple2[A1, A2](a1: =>F[A1], a2: =>F[A2]): F[(A1, A2)] = divide2(a1, a2)(identity)

  def divide2[A1, A2, Z](a1: =>F[A1], a2: =>F[A2])(f: Z => (A1, A2)): F[Z]
}

object Divide {
  def apply[F[_]](implicit ev: Divide[F]): Divide[F] = ev

  def fromIso[F[_], G[_]](i: F <~> G)(implicit D: Divide[G]): Divide[F] =
    new Divide[F] with IsomorphismContravariant[F, G] {
      implicit val G: Contravariant[G] = D
      val iso: F <~> G = i

      def divide2[A1, A2, Z](a1: => F[A1], a2: => F[A2])(f: Z => (A1, A2)): F[Z] =
        iso.from(D.divide2(iso.to(a1), iso.to(a2))(f))
    }

  implicit def divideFunction1[O](implicit S: Semigroup[O]): Divide[? => O] = new Divide[? => O] {
    def contramap[A, B](r: A => O)(f: B => A): B => O = b => r(f(b))
    def divide2[A1, A2, Z](a1: => A1 => O, a2: => A2 => O)(f: Z => (A1, A2)): Z => O = z => {
      val (x, y) = f(z)
      S.append(a1(x), a2(y))
    }
  }
}
