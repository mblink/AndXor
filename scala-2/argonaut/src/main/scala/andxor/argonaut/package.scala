package andxor

import _root_.argonaut.{DecodeJson, DecodeResult, EncodeJson, HCursor, Json}
import cats.{~>, Apply, Monoid}

trait LPArgonaut {
  trait JsonSumCodec[A] {
    def encodeField(name: String, value: Json): Json
    def decodeField(name: String, cursor: HCursor, decode: DecodeJson[A]): DecodeResult[A]
  }
  object JsonSumCodec {
    class ObjectCodec[A] extends JsonSumCodec[A] {
      def toJsonName(name: String): String = name
      def encodeField(name: String, value: Json): Json = Json(toJsonName(name) -> value)
      def decodeField(name: String, cursor: HCursor, decode: DecodeJson[A]): DecodeResult[A] =
        cursor.get[A](toJsonName(name))(decode)
    }

    class TypeFieldCodec[A] extends JsonSumCodec[A] {
      def typeField: String = "type"
      def toTypeValue(name: String): String = name
      def encodeField(name: String, value: Json): Json = (typeField -> Json.jString(toTypeValue(name))) ->: value
      def decodeField(name: String, cursor: HCursor, decode: DecodeJson[A]): DecodeResult[A] =
        cursor.get[String](typeField).result match {
          case Right(name0) if name0 == toTypeValue(name) => decode.decode(cursor)
          case _ => DecodeResult.fail(s"Invalid type, expected $name", cursor.history)
        }
    }

    implicit def default[A]: JsonSumCodec[A] = new ObjectCodec[A]
  }

  implicit def encodeJsonBaseAdtVal[A <: Singleton, L <: Singleton with String](
    implicit codec: JsonSumCodec[A],
    label: ValueOf[L]
  ): EncodeJson[Labelled[A, L]] =
    EncodeJson(a => codec.encodeField(a.label, Json()))

  implicit def decodeJsonBaseAdtVal[A <: Singleton, L <: Singleton with String](
    implicit value: ValueOf[Labelled[A, L]],
    codec: JsonSumCodec[Labelled[A, L]],
    label: ValueOf[L]
  ): DecodeJson[Labelled[A, L]] =
    DecodeJson(codec.decodeField(value.value.label, _, DecodeJson(_ => DecodeResult.ok(value.value))))
}

package object argonaut extends LPArgonaut {
  implicit val jsonMonoid: Monoid[Json] = Monoid.instance(Json(), _.deepmerge(_))

  implicit def encodeJsonLabelled[L <: Singleton with String, A](
    implicit ej: EncodeJson[A],
    label: ValueOf[L]
  ): EncodeJson[Labelled[A, L]] =
    EncodeJson(l => Json(l.label -> ej(l.value)))

  implicit def encodeJsonAdtVal[A <: Singleton, L <: Singleton with String](
    implicit ej: EncodeJson[A],
    codec: JsonSumCodec[A],
    label: ValueOf[L]
  ): EncodeJson[Labelled[A, L]] =
    EncodeJson(a => codec.encodeField(a.label, ej.encode(a.value)))

  type EncodeJsonF[A] = A => Json

  implicit val encodeJsonToEncodeJsonF: EncodeJson ~> EncodeJsonF = Lambda[EncodeJson ~> EncodeJsonF](_.encode _)
  implicit val encodeJsonFToEncodeJson: EncodeJsonF ~> EncodeJson = Lambda[EncodeJsonF ~> EncodeJson](EncodeJson(_))

  implicit val encodeJsonDivide: Divide[EncodeJson] = Divide.fromIso(encodeJsonToEncodeJsonF, encodeJsonFToEncodeJson)
  implicit val encodeJsonDecide: Decidable[EncodeJson] = Decidable.fromIso(encodeJsonToEncodeJsonF, encodeJsonFToEncodeJson)

  implicit def decodeJsonLabelled[L <: Singleton with String, A: DecodeJson](implicit label: ValueOf[L]): DecodeJson[Labelled[A, L]] =
    DecodeJson(_.get[A](label.value).map(Labelled[A, L](_)))

  implicit val decodeJsonApply: Apply[DecodeJson] = new Apply[DecodeJson] {
    def map[A, B](fa: DecodeJson[A])(f: A => B): DecodeJson[B] = fa.map(f)
    def ap[A, B](f: DecodeJson[A => B])(fa: DecodeJson[A]): DecodeJson[B] =
      DecodeJson(c => f(c).flatMap(g => fa(c).map(g(_))))
  }

  implicit val decodeJsonAlt: Alt[DecodeJson] = new Alt[DecodeJson] {
    def map[A, B](fa: DecodeJson[A])(f: A => B): DecodeJson[B] = fa.map(f)
    def alt[A](a1: DecodeJson[A], a2: DecodeJson[A]): DecodeJson[A] = a1 ||| a2
  }

  val decodeJsonDeriving: AndXorIso.DerivingLabelledCovariant[DecodeJson] = new AndXorIso.DerivingLabelledCovariant[DecodeJson] {}
  implicit def decodeJsonToDeriving(@annotation.unused a: DecodeJson.type): decodeJsonDeriving.type = decodeJsonDeriving

  val encodeJsonDeriving: AndXorIso.DerivingLabelledContravariant[EncodeJson] = new AndXorIso.DerivingLabelledContravariant[EncodeJson] {}
  implicit def encodeJsonToDeriving(@annotation.unused a: EncodeJson.type): encodeJsonDeriving.type = encodeJsonDeriving
}
