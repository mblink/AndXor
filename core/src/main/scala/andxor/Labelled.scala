package andxor

import argonaut.{DecodeJson, EncodeJson, Json}
import argonaut.Argonaut._
import scalaz.Apply
import scalaz.Id.Id
import shapeless.{Witness => W}

trait Labelled[A] {
  type L <: W.Lt[_ <: Symbol]
  val label: L
  val value: A
}

object Labelled {
  type Aux[L0 <: Symbol, A] = Labelled[A] { type L = W.Aux[L0] }

  def apply[A](label0: W.Lt[_ <: Symbol], value0: A): Labelled.Aux[label0.T, A] = new Labelled[A] {
    type L = W.Aux[label0.T]
    val label = label0
    val value: A = value0
  }

  implicit def encodeJsonLabelled[L <: Symbol, A: EncodeJson]: EncodeJson[Labelled.Aux[L, A]] =
    EncodeJson(l => Json(l.label.value.name := l.value))

  implicit def decodeJsonLabelled[L <: Symbol, A: DecodeJson](implicit w: W.Aux[L]): DecodeJson[Labelled.Aux[L, A]] =
    DecodeJson(_.get[A](w.value.name).map(Labelled(w, _)))
}

object test {
  object Test extends AndXorF3[
    Labelled.Aux[W.`'str`.T, String],
    Labelled.Aux[W.`'int`.T, Int],
    Labelled.Aux[W.`'bool`.T, Boolean]
  ] {
    def apply(str: String, int: Int, bool: Boolean): Repr[Id]#Prod = (
      Labelled('str, str),
      Labelled('int, int),
      Labelled('bool, bool)
    )
  }
  type Test = Test.Repr[Id]#Prod

  implicit val divEj: Divide[EncodeJson] = new Divide[EncodeJson] {
    def contramap[A, B](fa: EncodeJson[A])(f: B => A) = fa.contramap(f)
    def divide2[A1, A2, Z](fa1: => EncodeJson[A1], fa2: => EncodeJson[A2])(f: Z => (A1, A2)): EncodeJson[Z] =
      EncodeJson { z =>
        val (a1, a2) = f(z)
        fa1.encode(a1).deepmerge(fa2.encode(a2))
      }
  }

  implicit val apDj: Apply[DecodeJson] = new Apply[DecodeJson] {
    def map[A, B](fa: DecodeJson[A])(f: A => B): DecodeJson[B] = fa.map(f)
    def ap[A, B](fa: => DecodeJson[A])(f: => DecodeJson[A => B]): DecodeJson[B] =
      DecodeJson(c => f(c).flatMap(g => fa(c).map(g(_))))
  }

  implicit val encodeJsonTest: EncodeJson[Test] = Test[Id].combine[EncodeJson].divide
  implicit val decodeJsonTest: DecodeJson[Test] = Test[Id].combine[DecodeJson].apply
}
