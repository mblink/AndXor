// package andxor

// import andxor.types._
// // import scala.language.existentials
// import scalaz.{~>, Applicative, Monoid}

// object rank2 {
//   type FixedNT[A[_], B] = A ~> ({ type X[T] = B })#X
//   type TraverseNT[A[_], G[_], B[_]] = A ~> ({ type X[T] = G[B[T]] })#X
//   type BindNT[F[_[_]], A[_], B[_]] = A ~> ({ type X[T] = F[B] })#X

//   trait Arrow1[A[_], B[_], C] {
//     def apply(ac: A[C]): B[C]
//   }

//   trait Functor1[F[_[_]]] {
//     def map[A[_], B[_]](fa: F[A])(f: A ~> B): F[B]
//   }

//   trait Apply1[F[_[_]]] extends Functor1[F] {
//     def ap[A[_], B[_]](fa: => F[A])(fab: F[Arrow1[A, B, ?]]): F[B]
//   }

//   trait Applicative1[F[_[_]]] extends Apply1[F] {
//     def point[A[_]](a: A[T] forSome { type T }): F[A]
//   }

//   trait Foldable1[F[_[_]]] {
//     def foldMap[M: Monoid, A[_]](fa: F[A])(f: FixedNT[A, M]): M
//   }

//   trait Traversable1[F[_[_]]] extends Functor1[F] with Foldable1[F] {
//     def traverse[G[_]: Applicative, A[_], B[_]](fa: F[A])(f: TraverseNT[A, G, B]): G[F[B]]
//     def sequence[G[_]: Applicative, A[_]](fa: F[Lambda[t => G[A[t]]]]): G[F[A]]
//   }

//   trait Monad1[F[_[_]]] {
//     def bind[A[_], B[_]](fa: F[A])(f: BindNT[F, A, B]): F[B]
//   }

//   // sealed trait Free1[W[_[_]], F[_]]
//   // case class Pure1[W[_[_]], F[_], A](fa: F[A]) extends Free1[W, F]
//   // case class Impure1[W[_[_]], F[_], G[_], A, B](fa: Free1[W, F, A], f: F ~> Free1[W, G, ?]) extends Free1[W, F, A]

//   // object Free1 {
//   //   implicit def free1Monad[F[_], T]: Monad1[Free1[F, ?[_], A]] = new Monad1[Free1[F, ?[_], T]] {
//   //     def bind[A[_], B[_]]
//   //   }
//   // }

//   implicit def prod1Inst[A1]: Functor1[Prod1[?[_], A1]] with Apply1[Prod1[?[_], A1]] with Traversable1[Prod1[?[_], A1]] with Monad1[Prod1[?[_], A1]] =
//     new Functor1[Prod1[?[_], A1]] with Apply1[Prod1[?[_], A1]] with Traversable1[Prod1[?[_], A1]] with Monad1[Prod1[?[_], A1]] {
//       def map[A[_], B[_]](fa: Prod1[A, A1])(f: A ~> B): Prod1[B, A1] = Prod1[B, A1](f(fa.run))
//       def ap[A[_], B[_]](fa: => Prod1[A, A1])(fab: Prod1[Arrow1[A, B, ?], A1]): Prod1[B, A1] = Prod1[B, A1](fab.run(fa.run))
//       // def point[A[_], T](a: A[T]): F[A]
//       // def point[A[_]: Applicative]: Prod1[A, A1] = Prod1[A, A1](Monoid[A1].zero)
//       def point[A[_]](a: A[T] forSome { type T }): Prod1[A, A1] = Prod1[A, A1](a)

//       def foldMap[M: Monoid, A[_]](fa: Prod1[A, A1])(f: FixedNT[A, M]): M = f(fa.run)

//       def traverse[G[_]: Applicative, A[_], B[_]](fa: Prod1[A, A1])(f: TraverseNT[A, G, B]): G[Prod1[B, A1]] =
//         Applicative[G].map(f(fa.run))(Prod1[B, A1](_))

//       def sequence[G[_]: Applicative, A[_]](fa: Prod1[Lambda[t => G[A[t]]], A1]): G[Prod1[A, A1]] =
//         Applicative[G].map(fa.asInstanceOf[G[A[A1]]])(Prod1[A, A1](_))
//         // Applicative[G].map(fa.run)(x => x)

//       def bind[A[_], B[_]](fa: Prod1[A, A1])(f: BindNT[Prod1[?[_], A1], A, B]): Prod1[B, A1] =
//         f(fa.run)
//     }
// }
