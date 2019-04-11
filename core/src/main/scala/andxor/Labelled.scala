package andxor

import argonaut.{DecodeJson, EncodeJson, Json}
import argonaut.Argonaut._
import scalaz.Apply
import scalaz.Id.Id

trait Labelled[A] {
  type L <: Singleton with String
  val label: L
  val value: A
}

object Labelled {
  type Aux[L0 <: Singleton with String, A] = Labelled[A] { type L = L0 }

  def apply[L0 <: Singleton with String, A](label0: L0, value0: A): Labelled.Aux[L0, A] = new Labelled[A] {
    type L = L0
    val label = label0
    val value: A = value0
  }

  implicit def encodeJsonLabelled[L <: Singleton with String, A: EncodeJson]: EncodeJson[Labelled.Aux[L, A]] =
    EncodeJson(l => Json(l.label := l.value))

  implicit def decodeJsonLabelled[L <: Singleton with String, A: DecodeJson](implicit v: ValueOf[L]): DecodeJson[Labelled.Aux[L, A]] =
    DecodeJson(_.get[A](v.value).map(Labelled(v.value, _)))
}

object test {
  object Test extends AndXorF3[
    Labelled.Aux["str", String],
    Labelled.Aux["int", Int],
    Labelled.Aux["bool", Boolean]
  ] {
    def apply(str: String, int: Int, bool: Boolean): Repr[Id]#Prod = (
      Labelled("str", str),
      Labelled("int", int),
      Labelled("bool", bool)
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
