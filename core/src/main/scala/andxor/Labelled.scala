package andxor

import scalaz.{Applicative, Equal, Traverse}
import scalaz.std.string._
import scalaz.std.tuple._

trait Labelled[A] {
  type L <: Singleton with String
  val label: L
  val value: A

  override def toString: String = s"Labelled($label, $value)"
}

object Labelled {
  type Aux[A, L0 <: Singleton with String] = Labelled[A] { type L = L0 }

  def apply[A, L0 <: Singleton with String](value0: A, label0: L0): Labelled.Aux[A, L0] = new Labelled[A] {
    type L = L0
    val label: L = label0
    val value: A = value0
  }

  implicit def traverseLabelled[L <: Singleton with String]: Traverse[Aux[?, L]] = new Traverse[Aux[?, L]] {
    def traverseImpl[G[_]: Applicative, A, B](fa: Aux[A, L])(f: A => G[B]): G[Aux[B, L]] =
      Applicative[G].map(f(fa.value))(Labelled(_, fa.label))
  }

  implicit def equalLabelled[A: Equal, L <: Singleton with String]: Equal[Aux[A, L]] =
    Equal.equalBy(l => ((l.label: String), l.value))
}
