package andxor

import scalaz.Lens

trait ForallF[T[_[_]]] { def apply[F[_]]: T[F] }

case class FLens[T[_[_]], A[_[_]]](run: ForallF[Lambda[f[_] => Lens[T[f], A[f]]]])
// object FLens {
//   type Lenses[T[_[_]]] = T[FLens[T, ?]]
// }
