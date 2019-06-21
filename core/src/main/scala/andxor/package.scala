import andxor.MapN.syntax._
import scalaz.{~>, Applicative, Lens, PLens}

package object andxor {
  type FInj[T[_[_]], A[_[_]], F[_]] = Inj[T[F], A[F]]
  type FLens[T[_[_]], A[_[_]], F[_]] = Lens[T[F], A[F]]
  type FPLens[T[_[_]], A[_[_]], F[_]] = PLens[T[F], A[F]]
  type FConst[A] = { type T[F[_]] >: F[A] <: F[A] }

  implicit def FConstInstance[X]: FFunctor[FConst[X]#T] with FTraverse[FConst[X]#T] with FoldMap[FConst[X]#T, FConst[X]#T] =
    new FFunctor[FConst[X]#T] with FTraverse[FConst[X]#T] with FoldMap[FConst[X]#T, FConst[X]#T] {
      type T[F[_]] = FConst[X]#T[F]

      def map[A[_], B[_]](fa: T[A])(f: A ~> B): T[B] = f(fa)

      def traverse[F[_], G[_], A[_]: Applicative](tf: T[F])(f: F ~> Lambda[a => A[G[a]]]): A[T[G]] =
        map[F, Lambda[a => A[G[a]]]](tf)(f)

      def unconsAll[F[_], G[_]](p: T[F])(implicit U: Uncons[F, G]): (List[T[G]], T[F]) = U(p).map1(_.toList)

      def unconsOne[F[_], G[_]](p: T[F], c: T[G])(implicit U: Uncons[F, G]): (Option[T[G]], T[F]) = U(p)
    }

  implicit def FInjConst[T[_[_]], F[_], A](implicit i: FInj[T, FConst[A]#T, F]): Inj[T[F], F[A]] = i
  implicit def FLensConst[T[_[_]], F[_], A](implicit l: FLens[T, FConst[A]#T, F]): Lens[T[F], F[A]] = l
  implicit def FPLensConst[T[_[_]], F[_], A](implicit l: FPLens[T, FConst[A]#T, F]): PLens[T[F], F[A]] = l
}
