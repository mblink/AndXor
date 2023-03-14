package andxor

import cats.{~>, Eq}

trait FFunctor[F[_[_]]] {
  def map[A[_], B[_]](fa: F[A])(f: A ~> B): F[B]

  trait FFunctorLaw {
    final def identity[A[_]](fa: F[A])(using FA: Eq[F[A]]): Boolean =
      FA.eqv(map(fa)(new (A ~> A) { def apply[a](aa: A[a]): A[a] = aa }), fa)

    final def composite[A[_], B[_], C[_]](fa: F[A], f1: A ~> B, f2: B ~> C)(using FC: Eq[F[C]]): Boolean =
      FC.eqv(map(map(fa)(f1))(f2), map(fa)(f2.compose(f1)))
  }
  final lazy val ffunctorLaw = new FFunctorLaw {}
}

object FFunctor {
  @inline def apply[F[_[_]]](using ev: FFunctor[F]): FFunctor[F] = ev

  private[andxor] trait FX[X] extends FFunctor[[F[_]] =>> F[X]] {
    def map[A[_], B[_]](fa: A[X])(f: A ~> B): B[X] =
      f(fa)
  }

  private[andxor] trait Tuple1[X] extends FFunctor[[F[_]] =>> F[X] *: EmptyTuple] {
    def map[A[_], B[_]](fa: A[X] *: EmptyTuple)(f: A ~> B): B[X] *: EmptyTuple =
      f(fa.head) *: EmptyTuple
  }

  private[andxor] trait TupleN[H, T[_[_]] <: Tuple] extends FFunctor[[F[_]] =>> F[H] *: T[F]] {
    protected val FT: FFunctor[T]
    def map[A[_], B[_]](fa: A[H] *: T[A])(f: A ~> B): B[H] *: T[B] =
      f(fa.head) *: FT.map(fa.tail)(f)
  }

  private[andxor] trait Either[L, R[_[_]]] extends FFunctor[[F[_]] =>> F[L] |: R[F]] {
    protected val FR: FFunctor[R]
    def map[A[_], B[_]](fa: A[L] |: R[A])(f: A ~> B): B[L] |: R[B] =
      fa match {
        case Left(al) => Left(f(al))
        case Right(ra) => Right(FR.map(ra)(f))
      }
  }

  given fx[X]: FFunctor[[F[_]] =>> F[X]] =
    new FX[X] {}

  given tuple1[X]: FFunctor[[F[_]] =>> F[X] *: EmptyTuple] =
    new Tuple1[X] {}

  given tupleN[H, T[_[_]] <: Tuple](using F: FFunctor[T]): FFunctor[[F[_]] =>> F[H] *: T[F]] =
    new TupleN[H, T] { protected val FT = F }

  given either[L, R[_[_]]](using F: FFunctor[R]): FFunctor[[F[_]] =>> F[L] |: R[F]] =
    new Either[L, R] { protected val FR = F }
}
