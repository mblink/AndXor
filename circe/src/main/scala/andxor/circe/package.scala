package andxor

import andxor.tags._
import _root_.io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import scalaz.{~>, @@, Apply, Monoid, Tag}
import scalaz.Isomorphism.IsoFunctor

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

  implicit def encoderBaseAdtVal[A, L <: Singleton with String](implicit codec: JsonSumCodec[A]): Encoder[Labelled.Aux[A @@ ADTValue, L]] =
    Encoder.instance(a => codec.encodeField(a.label, Json.obj()))

  implicit def decoderBaseAdtVal[A, L <: Singleton with String](
    implicit value: Labelled.Aux[A @@ ADTValue, L],
    codec: JsonSumCodec[Labelled.Aux[A @@ ADTValue, L]]
  ): Decoder[Labelled.Aux[A @@ ADTValue, L]] =
    Decoder.instance(codec.decodeField(value.label, _, Decoder.instance(_ => Right(value))))
}

package object circe extends LPCirce {
  implicit val jsonMonoid: Monoid[Json] = Monoid.instance[Json](_.deepMerge(_), Json.obj())

  implicit def encoderLabelled[L <: Singleton with String, A](implicit e: Encoder[A]): Encoder[Labelled.Aux[A, L]] =
    Encoder.instance(l => Json.obj(l.label -> e(l.value)))

  implicit def encoderAdtVal[A, L <: Singleton with String](implicit e: Encoder[A], codec: JsonSumCodec[A]): Encoder[Labelled.Aux[A @@ ADTValue, L]] =
    Encoder.instance(a => codec.encodeField(a.label, e(Tag.of[ADTValue].unwrap(a.value))))

  type EncoderF[A] = A => Json

  implicit val encoderIso: IsoFunctor[Encoder, EncoderF] =
    IsoFunctor[Encoder, EncoderF](Lambda[Encoder ~> EncoderF](_.apply _), Lambda[EncoderF ~> Encoder](Encoder.instance(_)))

  implicit val encoderDivide: Divide[Encoder] = Divide.fromIso(encoderIso)
  implicit val encoderDecide: Decidable[Encoder] = Decidable.fromIso(encoderIso)

  implicit def decoderLabelled[L <: Singleton with String, A: Decoder](implicit label: L): Decoder[Labelled.Aux[A, L]] =
    Decoder.instance(_.get[A](label).map(Labelled(_, label)))

  implicit val decoderApply: Apply[Decoder] = new Apply[Decoder] {
    def map[A, B](fa: Decoder[A])(f: A => B): Decoder[B] = fa.map(f)
    def ap[A, B](fa: => Decoder[A])(f: => Decoder[A => B]): Decoder[B] =
      Decoder(c => f(c).flatMap(g => fa(c).map(g(_))))
  }

  implicit val decoderAlt: Alt[Decoder] = new Alt[Decoder] {
    def point[A](a: => A): Decoder[A] = Decoder.instance(_ => Right(a))
    def ap[A, B](fa: => Decoder[A])(f: => Decoder[A => B]): Decoder[B] =
      Decoder.instance(c => f(c).flatMap(g => fa(c).map(g(_))))
    def alt[A](a1: => Decoder[A], a2: => Decoder[A]): Decoder[A] = a1.or(a2)
  }
}
