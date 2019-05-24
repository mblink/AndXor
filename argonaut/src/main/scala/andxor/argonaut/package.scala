package andxor

import andxor.tags._
import _root_.argonaut.{DecodeJson, EncodeJson, Json}
import scalaz.{~>, @@, Apply, Monoid}
import scalaz.Isomorphism.IsoFunctor

package object argonaut {
  implicit val jsonMonoid: Monoid[Json] = Monoid.instance[Json](_.deepmerge(_), Json())

  implicit def encodeJsonLabelled[L <: Singleton with String, A](implicit ej: EncodeJson[A]): EncodeJson[Labelled.Aux[A, L]] =
    EncodeJson(l => Json(l.label -> ej(l.value)))

  implicit def encodeJsonAdtVal[A, L <: Singleton with String]: EncodeJson[Labelled.Aux[A @@ ADTValue, L]] =
    EncodeJson(a => Json(a.label -> Json()))

  type EncodeJsonF[A] = A => Json

  implicit val encodeJsonIso: IsoFunctor[EncodeJson, EncodeJsonF] =
    IsoFunctor[EncodeJson, EncodeJsonF](
      Lambda[EncodeJson ~> EncodeJsonF](_.encode _),
      Lambda[EncodeJsonF ~> EncodeJson](EncodeJson(_)))

  implicit val encodeJsonDivide: Divide[EncodeJson] = Divide.fromIso(encodeJsonIso)
  implicit val encodeJsonDecide: Decidable[EncodeJson] = Decidable.fromIso(encodeJsonIso)

  implicit def decodeJsonLabelled[L <: Singleton with String, A: DecodeJson](implicit l: L): DecodeJson[Labelled.Aux[A, L]] =
    DecodeJson(_.get[A](l).map(Labelled(_, l)))

  implicit val decodeJsonApply: Apply[DecodeJson] = new Apply[DecodeJson] {
    def map[A, B](fa: DecodeJson[A])(f: A => B): DecodeJson[B] = fa.map(f)
    def ap[A, B](fa: => DecodeJson[A])(f: => DecodeJson[A => B]): DecodeJson[B] =
      DecodeJson(c => f(c).flatMap(g => fa(c).map(g(_))))
  }
}
