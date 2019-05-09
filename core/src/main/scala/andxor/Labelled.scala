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

/*
import andxor.AndXor2
import scalaz.Isomorphism.IsoSet

trait Labelled[A] {
  type L <: Singleton with String
  val label: L
  val value: A
}

object Labelled {
  type Aux[A, L0 <: String] = Labelled[A] { type L = L0 }

  def apply[A, L0 <: Singleton with String](value0: A, label0: L0): Labelled.Aux[A, L0] = new Labelled[A] {
    type L = L0
    val label: L = label0
    val value: A = value0
  }
}


case class Foo(x1: String, x2: Int)
object Foo {
  implicit val x1Label = "x1"
  implicit val x2Label = "x2"

  type AXO = AndXor2[Labelled.Aux[String, x1Label.type], Labelled.Aux[Int, x2Label.type]]
  val AXO: AXO = AndXor2[Labelled.Aux[String, x1Label.type], Labelled.Aux[Int, x2Label.type]]

  val iso: IsoSet[Foo, AXO.Prod] = IsoSet(
    x => AXO.Prod((Labelled[String, x1Label.type](x.x1, x1Label), Labelled[Int, x2Label.type](x.x2, x2Label))),
    x => new Foo(x.run._1.value, x.run._2.value))
}
*/
