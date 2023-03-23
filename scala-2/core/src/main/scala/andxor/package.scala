import andxor.tuple._
import cats.{~>, Applicative, MonoidK}

package object andxor {
  type FConst[A] = { type T[F[_]] >: F[A] <: F[A] }

  implicit def FConstInstance[X]: FFunctor[FConst[X]#T] with FTraverseProd[FConst[X]#T] with FoldMap[FConst[X]#T, FConst[X]#T] =
    new FFunctor[FConst[X]#T] with FTraverseProd[FConst[X]#T] with FoldMap[FConst[X]#T, FConst[X]#T] {
      type T[F[_]] = FConst[X]#T[F]

      def map[A[_], B[_]](fa: T[A])(f: A ~> B): T[B] = f(fa)

      def traverse[F[_], G[_], A[_]: Applicative](tf: T[F])(f: F ~> Lambda[a => A[G[a]]]): A[T[G]] =
        map[F, Lambda[a => A[G[a]]]](tf)(f)

      def emptyProd[F[_]](implicit PE: MonoidK[F]): T[F] = PE.empty

      def unconsAll[F[_], G[_]](p: T[F])(implicit U: Uncons[F, G]): (List[T[G]], T[F]) = U(p).map1(_.toList)

      def unconsOne[F[_], G[_]](p: T[F], c: T[G])(implicit U: Uncons[F, G]): (Option[T[G]], T[F]) = U(p)
    }

  type Labelled[A, L] = labelled.Labelled[A, L]
  val Labelled: labelled.Labelled.type = labelled.Labelled

  @inline def axo[A]: AndXor1[A] = AndXor[A]
  @inline def axoN[A[_[_]]]: AndXorNested1[A] = AndXorNested1[A]
}
