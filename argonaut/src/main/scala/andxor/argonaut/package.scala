package andxor

import _root_.argonaut.{DecodeJson, EncodeJson, Json}
import scalaz.Apply
import scalaz.syntax.id._
import shapeless.{Witness => W}

package object argonaut {
  implicit def encodeJsonLabelled[L <: String, A](implicit ej: EncodeJson[A]): EncodeJson[Labelled.Aux[A, L]] =
    EncodeJson(l => Json(l.label.value -> ej(l.value)))

  implicit val encodeJsonDivide: Divide[EncodeJson] = new Divide[EncodeJson] {
    def contramap[A, B](fa: EncodeJson[A])(f: B => A): EncodeJson[B] = fa.contramap(f)
    def divide2[A1, A2, Z](a1: => EncodeJson[A1], a2: => EncodeJson[A2])(f: Z => (A1, A2)): EncodeJson[Z] =
      EncodeJson(f(_) |> (t => a1(t._1).deepmerge(a2(t._2))))
  }

  implicit def decodeJsonLabelled[L <: String, A: DecodeJson](implicit w: W.Aux[L]): DecodeJson[Labelled.Aux[A, L]] =
    DecodeJson(_.get[A](w.value).map(Labelled(_, w)))

  implicit val decodeJsonApply: Apply[DecodeJson] = new Apply[DecodeJson] {
    def map[A, B](fa: DecodeJson[A])(f: A => B): DecodeJson[B] = fa.map(f)
    def ap[A, B](fa: => DecodeJson[A])(f: => DecodeJson[A => B]): DecodeJson[B] =
      DecodeJson(c => f(c).flatMap(g => fa(c).map(g(_))))
  }
}
