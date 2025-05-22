package andxor

import cats.{~>, Apply, Contravariant, Eq, Monoid, Semigroup}
import cats.data.Kleisli

trait Divide[F[_]] extends Contravariant[F] {
  final def divide[A, B, C](fa: F[A], fb: F[B])(f: C => (A, B)): F[C] = divide2(fa, fb)(f)
  final def divide1[A1, Z](a1: F[A1])(f: Z => A1): F[Z] = contramap(a1)(f)
  def tuple2[A1, A2](a1: F[A1], a2: F[A2]): F[(A1, A2)] = divide2(a1, a2)(identity)
  def divide2[A1, A2, Z](a1: F[A1], a2: F[A2])(f: Z => (A1, A2)): F[Z]
}

trait DivideLP {
  trait IsomorphismDivide[F[_], G[_]] extends Divide[F] {
    def G: Divide[G]
    def to: F ~> G
    def from: G ~> F
    override def contramap[A, B](fa: F[A])(f: B => A): F[B] = from(G.contramap(to(fa))(f))
    def divide2[A1, A2, Z](a1: F[A1], a2: F[A2])(f: Z => (A1, A2)): F[Z] = from(G.divide2(to(a1), to(a2))(f))
  }

  trait DivideFunction1[O] extends Divide[* => O] {
    def G: Semigroup[O]
    override def contramap[A, B](r: A => O)(f: B => A): B => O = b => r(f(b))
    def divide2[A1, A2, Z](a1: A1 => O, a2: A2 => O)(f: Z => (A1, A2)): Z => O = z => {
      val (x, y) = f(z)
      G.combine(a1(x), a2(y))
    }
  }

  implicit def divideFunction1[O](implicit S: Semigroup[O]): Divide[* => O] = new DivideFunction1[O] { val G = S }

  trait DivideKleisli[F[_], O] extends Divide[Kleisli[F, *, O]] {
    def F: Apply[F]
    def G: Semigroup[O]
    override def contramap[A, B](fa: Kleisli[F, A, O])(f: B => A): Kleisli[F, B, O] = Kleisli(b => fa.run(f(b)))
    def divide2[A1, A2, Z](a1: Kleisli[F, A1, O], a2: Kleisli[F, A2, O])(f: Z => (A1, A2)): Kleisli[F, Z, O] =
      Kleisli { z =>
        val (x1, x2) = f(z)
        F.map2(a1.run(x1), a2.run(x2))(G.combine(_, _))
      }
  }

  implicit def divideKleisli[F[_], O](implicit A: Apply[F], S: Semigroup[O]): Divide[Kleisli[F, *, O]] = new DivideKleisli[F, O] {
    val F = A
    val G = S
  }

  trait DivideEq extends Divide[Eq] {
    override def contramap[A, B](fa: Eq[A])(f: B => A): Eq[B] = Eq.by(f)(fa)
    def divide2[A1, A2, Z](a1: Eq[A1], a2: Eq[A2])(f: Z => (A1, A2)): Eq[Z] =
      Eq.instance { (z1, z2) =>
        val (s1, s2) = f(z1)
        val (t1, t2) = f(z2)
        a1.eqv(s1, t1) && a2.eqv(s2, t2)
      }
  }

  implicit val divideEq: Divide[Eq] = new DivideEq {}
}

object Divide extends DivideLP {
  @inline def apply[F[_]](implicit ev: Divide[F]): Divide[F] = ev

  def fromIso[F[_], G[_]](fg: F ~> G, gf: G ~> F)(implicit D: Divide[G]): Divide[F] = new IsomorphismDivide[F, G] {
    implicit val G: Divide[G] = D
    val to = fg
    val from = gf
  }
}

trait Divisible[F[_]] extends Divide[F] {
  def conquer[A]: F[A]
  def contramap[A, B](fa: F[A])(f: B => A): F[B] = divide(conquer[Unit], fa)((b: B) => ((), f(b)))
}

object Divisible extends DivideLP {
  def apply[F[_]](implicit ev: Divisible[F]): Divisible[F] = ev

  def fromIso[F[_], G[_]](fg: F ~> G, gf: G ~> F)(implicit D: Divisible[G]): Divisible[F] =
    new Divisible[F] with IsomorphismDivide[F, G] {
      implicit val G: Divide[G] = D
      val to = fg
      val from = gf
      def conquer[A]: F[A] = from(D.conquer[A])
    }

  implicit def divisibleFunction1[O](implicit M: Monoid[O]): Divisible[* => O] = new Divisible[* => O] with DivideFunction1[O] {
    val G: Semigroup[O] = M
    def conquer[A]: A => O = _ => M.empty
  }

  implicit def divisibleKleisli[F[_], O](implicit A: Apply[F], M: Monoid[O]): Divide[Kleisli[F, *, O]] = new DivideKleisli[F, O] {
    val F = A
    val G: Semigroup[O] = M
  }

  implicit val divisibleEq: Divisible[Eq] = new Divisible[Eq] with DivideEq {
    def conquer[A]: Eq[A] = Eq.instance((_, _) => true)
  }
}
