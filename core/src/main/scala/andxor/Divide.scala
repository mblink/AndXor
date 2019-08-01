package andxor

import scalaz.{Apply, Contravariant, Equal, IsomorphismContravariant, Kleisli, Monoid, Semigroup}
import scalaz.Isomorphism.<~>

trait Divide[F[_]] extends Contravariant[F] {
  final def divide[A, B, C](fa: =>F[A], fb: =>F[B])(f: C => (A, B)): F[C] = divide2(fa, fb)(f)
  final def divide1[A1, Z](a1: F[A1])(f: Z => A1): F[Z] = contramap(a1)(f)
  def tuple2[A1, A2](a1: =>F[A1], a2: =>F[A2]): F[(A1, A2)] = divide2(a1, a2)(identity)
  def divide2[A1, A2, Z](a1: =>F[A1], a2: =>F[A2])(f: Z => (A1, A2)): F[Z]
}

trait DivideLP {
  trait IsomorphismDivide[F[_], G[_]] extends Divide[F] with IsomorphismContravariant[F, G] {
    override def G: Divide[G]
    def iso: F <~> G
    def divide2[A1, A2, Z](a1: => F[A1], a2: => F[A2])(f: Z => (A1, A2)): F[Z] = iso.from(G.divide2(iso.to(a1), iso.to(a2))(f))
  }

  trait DivideFunction1[O] extends Divide[? => O] {
    def G: Semigroup[O]
    override def contramap[A, B](r: A => O)(f: B => A): B => O = b => r(f(b))
    def divide2[A1, A2, Z](a1: => A1 => O, a2: => A2 => O)(f: Z => (A1, A2)): Z => O = z => {
      val (x, y) = f(z)
      G.append(a1(x), a2(y))
    }
  }

  implicit def divideFunction1[O](implicit S: Semigroup[O]): Divide[? => O] = new DivideFunction1[O] { val G = S }

  trait DivideKleisli[F[_], O] extends Divide[Kleisli[F, ?, O]] {
    def F: Apply[F]
    def G: Semigroup[O]
    override def contramap[A, B](fa: Kleisli[F, A, O])(f: B => A): Kleisli[F, B, O] = Kleisli(b => fa.run(f(b)))
    def divide2[A1, A2, Z](a1: => Kleisli[F, A1, O], a2: => Kleisli[F, A2, O])(f: Z => (A1, A2)): Kleisli[F, Z, O] =
      Kleisli { z =>
        val (x1, x2) = f(z)
        F.apply2(a1.run(x1), a2.run(x2))(G.append(_, _))
      }
  }

  implicit def divideKleisli[F[_], O](implicit A: Apply[F], S: Semigroup[O]): Divide[Kleisli[F, ?, O]] = new DivideKleisli[F, O] {
    val F = A
    val G = S
  }

  trait DivideEqual extends Divide[Equal] {
    override def contramap[A, B](fa: Equal[A])(f: B => A): Equal[B] = fa.contramap(f)
    def divide2[A1, A2, Z](a1: => Equal[A1], a2: => Equal[A2])(f: Z => (A1, A2)): Equal[Z] =
      Equal.equal { (z1, z2) =>
        val (s1, s2) = f(z1)
        val (t1, t2) = f(z2)
        a1.equal(s1, t1) && a2.equal(s2, t2)
      }
  }

  implicit val divideEqual: Divide[Equal] = new DivideEqual {}
}

object Divide extends DivideLP {
  def apply[F[_]](implicit ev: Divide[F]): Divide[F] = ev

  def fromIso[F[_], G[_]](i: F <~> G)(implicit D: Divide[G]): Divide[F] = new IsomorphismDivide[F, G] {
    implicit val G: Divide[G] = D
    val iso: F <~> G = i
  }
}

trait Divisible[F[_]] extends Divide[F] {
  def conquer[A]: F[A]
  def contramap[A, B](fa: F[A])(f: B => A): F[B] = divide(conquer[Unit], fa)((b: B) => ((), f(b)))
}

object Divisible extends DivideLP {
  def apply[F[_]](implicit ev: Divisible[F]): Divisible[F] = ev

  def fromIso[F[_], G[_]](i: F <~> G)(implicit D: Divisible[G]): Divisible[F] =
    new Divisible[F] with IsomorphismDivide[F, G] {
      implicit val G: Divide[G] = D
      val iso: F <~> G = i
      def conquer[A]: F[A] = iso.from(D.conquer[A])
    }

  implicit def divisibleFunction1[O](implicit M: Monoid[O]): Divisible[? => O] = new Divisible[? => O] with DivideFunction1[O] {
    val G: Semigroup[O] = M
    def conquer[A]: A => O = _ => M.zero
  }

  implicit def divisibleKleisli[F[_], O](implicit A: Apply[F], M: Monoid[O]): Divide[Kleisli[F, ?, O]] = new DivideKleisli[F, O] {
    val F = A
    val G: Semigroup[O] = M
  }

  implicit val divisibleEqual: Divisible[Equal] = new Divisible[Equal] with DivideEqual {
    def conquer[A]: Equal[A] = Equal.equal((_, _) => true)
  }
}
