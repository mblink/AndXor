package andxor

import scalaz.Equal
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

  implicit def equalLabelled[A: Equal, L <: Singleton with String]: Equal[Labelled.Aux[A, L]] =
    Equal.equalBy(l => ((l.label: String), l.value))
}
