package andxor

import cats.{Applicative, Eq, Eval, Traverse}
import cats.instances.string._
import cats.instances.tuple._

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
    def foldLeft[A, B](fa: Aux[A, L], b: B)(f: (B, A) => B): B = f(b, fa.value)
    def foldRight[A, B](fa: Aux[A, L], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = f(fa.value, lb)
    def traverse[G[_], A, B](fa: Aux[A, L])(f: A => G[B])(implicit G: Applicative[G]): G[Aux[B, L]] =
      G.map(f(fa.value))(Labelled(_, fa.label))
  }

  implicit def eqLabelled[A: Eq, L <: Singleton with String]: Eq[Aux[A, L]] =
    Eq.by(l => ((l.label: String), l.value))
}
