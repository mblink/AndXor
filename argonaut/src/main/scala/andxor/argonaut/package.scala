package andxor

import _root_.argonaut.{DecodeJson, DecodeResult, EncodeJson, HCursor, Json}
import cats.{~>, Apply, Id, Monoid}
import scala.compiletime.{summonAll, summonFrom}

sealed trait ArgonautLP {
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

    given default[A]: JsonSumCodec[A] = new ObjectCodec[A]
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

package object argonaut extends ArgonautLP {
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

  val encodeJsonToEncodeJsonF: EncodeJson ~> EncodeJsonF = new (EncodeJson ~> EncodeJsonF) {
    def apply[A](f: EncodeJson[A]): EncodeJsonF[A] = f.encode
  }
  val encodeJsonFToEncodeJson: EncodeJsonF ~> EncodeJson = new (EncodeJsonF ~> EncodeJson) {
    def apply[A](f: EncodeJsonF[A]): EncodeJson[A] = EncodeJson(f)
  }

  given encodeJsonDivide: Divide[EncodeJson] = Divide.fromIso(encodeJsonToEncodeJsonF, encodeJsonFToEncodeJson)
  given encodeJsonDecide: Decidable[EncodeJson] = Decidable.fromIso(encodeJsonToEncodeJsonF, encodeJsonFToEncodeJson)

  extension (x: EncodeJson.type) inline def derived[A]: EncodeJson[A] =
    summonFrom {
      case p: AndXorProdIso[A] =>
        given axoInstances: AndXorInstances[EncodeJson, p.Prod[Id]] =
          AndXorInstances(summonAll[Tuple.Map[p.Prod[Id], EncodeJson]])

        p.deriving[EncodeJson].divide

      case c: AndXorCopIso[A] =>
        given axoInstances: AndXorInstances[EncodeJson, c.Prod[Id]] =
          AndXorInstances(summonAll[Tuple.Map[c.Prod[Id], EncodeJson]])

        c.deriving[EncodeJson].choose
    }

  implicit def decodeJsonLabelled[L <: Singleton with String, A: DecodeJson](implicit label: ValueOf[L]): DecodeJson[Labelled[A, L]] =
    DecodeJson(_.get[A](label.value).map(Labelled[A, L](_)))

  implicit val decodeJsonInstance: Alt[DecodeJson] with Apply[DecodeJson] = new Alt[DecodeJson] with Apply[DecodeJson] {
    def map[A, B](fa: DecodeJson[A])(f: A => B): DecodeJson[B] = fa.map(f)
    def alt[A](a1: DecodeJson[A], a2: DecodeJson[A]): DecodeJson[A] = a1 ||| a2
    def ap[A, B](f: DecodeJson[A => B])(fa: DecodeJson[A]): DecodeJson[B] =
      DecodeJson(c => f(c).flatMap(g => fa(c).map(g(_))))
  }

  extension (x: DecodeJson.type) inline def derived[A]: DecodeJson[A] =
    summonFrom {
      case p: AndXorProdIso[A] =>
        given axoInstances: AndXorInstances[DecodeJson, p.Prod[Id]] =
          AndXorInstances(summonAll[Tuple.Map[p.Prod[Id], DecodeJson]])

        p.deriving[DecodeJson].apply

      case c: AndXorCopIso[A] =>
        given axoInstances: AndXorInstances[DecodeJson, c.Prod[Id]] =
          AndXorInstances(summonAll[Tuple.Map[c.Prod[Id], DecodeJson]])

        c.deriving[DecodeJson].alt
    }
}
