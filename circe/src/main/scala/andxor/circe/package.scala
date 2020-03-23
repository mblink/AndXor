package andxor

import andxor.types.ADTValue
import _root_.io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import cats.{~>, Apply, Monoid}

trait LPCirce {
  trait JsonSumCodec[A] {
    def encodeField(name: String, value: Json): Json
    def decodeField(name: String, cursor: HCursor, decode: Decoder[A]): Decoder.Result[A]
  }
  object JsonSumCodec {
    class ObjectCodec[A] extends JsonSumCodec[A] {
      def toJsonName(name: String): String = name
      def encodeField(name: String, value: Json): Json = Json.obj(toJsonName(name) -> value)
      def decodeField(name: String, cursor: HCursor, decode: Decoder[A]): Decoder.Result[A] =
        cursor.get[A](toJsonName(name))(decode)
    }

    class TypeFieldCodec[A] extends JsonSumCodec[A] {
      def typeField: String = "type"
      def toTypeValue(name: String): String = name
      def encodeField(name: String, value: Json): Json =
        value.mapObject((typeField -> Json.fromString(toTypeValue(name))) +: _)
      def decodeField(name: String, cursor: HCursor, decode: Decoder[A]): Decoder.Result[A] =
        cursor.get[String](typeField) match {
          case Right(name0) if name0 == toTypeValue(name) => decode(cursor)
          case _ => Left(DecodingFailure(s"Invalid type, expected $name", cursor.history))
        }
    }

    implicit def default[A]: JsonSumCodec[A] = new ObjectCodec[A]
  }

  implicit def encoderBaseAdtVal[A <: Singleton, L <: Singleton with String](implicit codec: JsonSumCodec[A]): Encoder[Labelled.Aux[ADTValue[A], L]] =
    Encoder.instance(a => codec.encodeField(a.label, Json.obj()))

  implicit def decoderBaseAdtVal[A <: Singleton, L <: Singleton with String](
    implicit value: Labelled.Aux[ADTValue[A], L],
    codec: JsonSumCodec[Labelled.Aux[ADTValue[A], L]]
  ): Decoder[Labelled.Aux[ADTValue[A], L]] =
    Decoder.instance(codec.decodeField(value.label, _, Decoder.instance(_ => Right(value))))
}

package object circe extends LPCirce {
  implicit val jsonMonoid: Monoid[Json] = new Monoid[Json] {
    lazy val empty = Json.obj()
    def combine(j1: Json, j2: Json): Json = j1.deepMerge(j2)
  }

  implicit def encoderLabelled[L <: Singleton with String, A](implicit e: Encoder[A]): Encoder[Labelled.Aux[A, L]] =
    Encoder.instance(l => Json.obj(l.label -> e(l.value)))

  implicit def encoderAdtVal[A <: Singleton, L <: Singleton with String](implicit e: Encoder[A], codec: JsonSumCodec[A]): Encoder[Labelled.Aux[ADTValue[A], L]] =
    Encoder.instance(a => codec.encodeField(a.label, e(a.value.value)))

  type EncoderF[A] = A => Json

  implicit val encoderToEncoderF: Encoder ~> EncoderF = Lambda[Encoder ~> EncoderF](_.apply _)
  implicit val encoderFToEncoder: EncoderF ~> Encoder = Lambda[EncoderF ~> Encoder](Encoder.instance(_))

  implicit val encoderDivide: Divide[Encoder] = Divide.fromIso(encoderToEncoderF, encoderFToEncoder)
  implicit val encoderDecide: Decidable[Encoder] = Decidable.fromIso(encoderToEncoderF, encoderFToEncoder)

  implicit def decoderLabelled[L <: Singleton with String, A: Decoder](implicit label: L): Decoder[Labelled.Aux[A, L]] =
    Decoder.instance(_.get[A](label) match {
      case Right(x) => Right(Labelled(x, label))
      case Left(f) => Left(f)
    })

  implicit val decoderApply: Apply[Decoder] = new Apply[Decoder] with DecoderAp {
    def map[A, B](fa: Decoder[A])(f: A => B): Decoder[B] = fa.map(f)
    def ap[A, B](f: Decoder[A => B])(fa: Decoder[A]): Decoder[B] =
      Decoder.instance(c => f(c).flatMap(g => fa(c).map(g(_))))
  }

  implicit val decoderAlt: Alt[Decoder] = new Alt[Decoder] {
    def map[A, B](fa: Decoder[A])(f: A => B): Decoder[B] = fa.map(f)
    def alt[A](a1: Decoder[A], a2: Decoder[A]): Decoder[A] = a1.or(a2)
  }
}
