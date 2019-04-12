package andxor

import scala.language.higherKinds
import scalaz.{Contravariant, Functor}
import shapeless.{Lazy, Witness => W}

trait Labelled[A] {
  type L <: W.Lt[String]
  val label: L
  val value: A
}

object Labelled {
  type Aux[A, L0 <: String] = Labelled[A] { type L = W.Aux[L0] }

  def apply[A](value0: A, label0: W.Lt[String]): Labelled.Aux[A, label0.T] = new Labelled[A] {
    type L = W.Aux[label0.T]
    val label = label0
    val value: A = value0
  }

  def labelledCovariant[F[_], L <: String, A](implicit F: Functor[F], fa: Lazy[F[A]], w: W.Aux[L]): F[Labelled.Aux[A, w.T]] =
    F.map[A, Labelled.Aux[A, w.T]](fa.value)(Labelled[A](_, w))

  def labelledContravariant[F[_], L <: String, A](implicit F: Contravariant[F], fa: Lazy[F[A]]): F[Labelled.Aux[A, L]] =
    F.contramap[A, Labelled.Aux[A, L]](fa.value)(_.value)
}
