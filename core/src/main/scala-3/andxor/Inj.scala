package andxor

import cats.{Monoid, Semigroup}

trait Inj[Cop, A] {
  def apply(a: A): Cop
}

trait InjLP {
  trait DivideInj[Prod] extends Divide[Inj[Prod, *]] {
    def G: Semigroup[Prod]
    override def contramap[A, B](fa: Inj[Prod, A])(f: B => A): Inj[Prod, B] = Inj.instance(b => fa(f(b)))
    def divide2[A1, A2, Z](a1: Inj[Prod, A1], a2: Inj[Prod, A2])(f: Z => (A1, A2)): Inj[Prod, Z] =
      Inj.instance { z =>
        val (i1, i2) = f(z)
        G.combine(a1(i1), a2(i2))
      }
  }

  given divideInj[Prod](using S: Semigroup[Prod]): Divide[Inj[Prod, *]] = new DivideInj[Prod] { val G = S }

  given aToTuple1[A]: Inj[A *: EmptyTuple, A] =
    Inj.instance(_ *: EmptyTuple)

  given tuple1ToVectorA[A]: Inj[Vector[A], A *: EmptyTuple] =
    Inj.instance { case a *: _ => Vector(a) }
}

object Inj extends InjLP with TupleInj {
  inline def apply[Cop, A](using ev: Inj[Cop, A]): Inj[Cop, A] = ev

  def instance[A, B](ab: A => B): Inj[B, A] = new Inj[B, A] {
    def apply(a: A): B = ab(a)
  }

  private final val idInst: Inj[Any, Any] = instance(identity)

  given id[A]: Inj[A, A] = idInst.asInstanceOf[Inj[A, A]]

  given injEitherLeft[L, R]: Inj[L |: R, L] = instance(Left(_: L))

  given injEitherRight[L, R, A](using i: Inj[R, A]): Inj[L |: R, A] =
    instance((a: A) => Right(i(a)))

  given eitherToTupleN[L, R, T <: Tuple](
    using injR: Inj[T, R],
    M: Monoid[L *: T]
  ): Inj[L *: T, L |: R] =
    Inj.instance {
      case Left(l) => M.empty match { case _ *: t => l *: t }
      case Right(r) => M.empty.head *: injR(r)
    }

  given tupleNToVectorEither[H, T <: Tuple, R](
    using injT: Inj[Vector[R], T],
  ): Inj[Vector[H |: R], H *: T] =
    Inj.instance { case h *: t => Left(h) +: injT(t).map(Right(_)) }

  given decidableInj[Cop]: Decidable[Inj[Cop, *]] =
    new Decidable[Inj[Cop, *]] {
      def contramap[A, B](fa: Inj[Cop, A])(f: B => A): Inj[Cop, B] = instance(b => fa(f(b)))
      def choose2[Z, A1, A2](a1: Inj[Cop, A1], a2: Inj[Cop, A2])(f: Z => Either[A1, A2]): Inj[Cop, Z] =
        instance(f(_).fold(a1(_), a2(_)))
    }

  given divisibleInj[Prod](using M: Monoid[Prod]): Divisible[Inj[Prod, *]] =
    new Divisible[Inj[Prod, *]] with DivideInj[Prod] {
      val G = M
      def conquer[A]: Inj[Prod, A] = instance(_ => M.empty)
    }
}
