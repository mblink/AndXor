package andxor

import _root_.io.circe.{Decoder, Encoder, Json}
import scalaz.Apply
import scalaz.syntax.id._
import shapeless.{Witness => W}

package object circe {
  implicit def encoderLabelled[L <: String, A](implicit e: Encoder[A]): Encoder[Labelled.Aux[A, L]] =
    Encoder.instance(l => Json.obj(l.label.value -> e(l.value)))

  implicit val encoderDivide: Divide[Encoder] = new Divide[Encoder] {
    def contramap[A, B](fa: Encoder[A])(f: B => A): Encoder[B] = fa.contramap(f)
    def divide2[A1, A2, Z](a1: => Encoder[A1], a2: => Encoder[A2])(f: Z => (A1, A2)): Encoder[Z] =
      Encoder.instance(f(_) |> (t => a1(t._1).deepMerge(a2(t._2))))
  }

  implicit def decoderLabelled[L <: String, A: Decoder](implicit w: W.Aux[L]): Decoder[Labelled.Aux[A, L]] =
    Decoder.instance(_.get[A](w.value).map(Labelled(_, w)))

  implicit val decoderApply: Apply[Decoder] = new Apply[Decoder] {
    def map[A, B](fa: Decoder[A])(f: A => B): Decoder[B] = fa.map(f)
    def ap[A, B](fa: => Decoder[A])(f: => Decoder[A => B]): Decoder[B] =
      Decoder.instance(c => f(c).flatMap(g => fa(c).map(g(_))))
  }
}
