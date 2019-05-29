package andxor.test

import andxor.{Alt, Decidable, Divide, Labelled}
import andxor.argonaut._
import andxor.circe._
import andxor.tags._
import argonaut.{DecodeJson, EncodeJson}
import io.circe.{Decoder, Encoder}
import scala.annotation.Annotation
import scalaz.{@@, ~>, Apply, Cord, Show}
import scalaz.Isomorphism.IsoFunctor
import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.string._
import scalaz.syntax.apply._
import scalaz.syntax.std.string._

object annotations {
  final class deriving(
    val covariant: Vector[AnyRef] = Vector(),
    val labelledCovariant: Vector[AnyRef] = Vector(),
    val contravariant: Vector[AnyRef] = Vector(),
    val labelledContravariant: Vector[AnyRef] = Vector(),
  ) extends Annotation
}

object typeclasses {
  implicit def showLabelled[A: Show, L <: Singleton with String]: Show[Labelled.Aux[A, L]] =
    Show.shows(l => s"""${l.label} := ${Show[A].shows(l.value)}""" ++ "\n")

  implicit def showAdtVal[A, L <: Singleton with String]: Show[Labelled.Aux[A @@ ADTValue, L]] =
    Show.shows(a => s"""ADTValue := ${a.label}""" ++ "\n")

  type ShowF[A] = A => Cord
  implicit val showIso: IsoFunctor[Show, ShowF] =
    IsoFunctor[Show, ShowF](Lambda[Show ~> ShowF](_.show _), Lambda[ShowF ~> Show](Show.show(_)))

  implicit val showDecide: Decidable[Show] = Decidable.fromIso[Show, ShowF](showIso)
  implicit val showDivide: Divide[Show] = Divide.fromIso[Show, ShowF](showIso)

  trait Read[A] { def read(s: String): Option[A] }
  object Read {
    implicit def readLabelled[A, L <: Singleton with String](implicit l: L, r: Read[A]): Read[Labelled.Aux[A, L]] =
      _.split("\n").flatMap(_.split(s"$l := ", 2).lift(1).flatMap(r.read(_))).headOption.map(Labelled(_, l))

    implicit def readAdtVal[A, L <: Singleton with String](
      implicit label: L,
      value: Labelled.Aux[A @@ ADTValue, L]
    ): Read[Labelled.Aux[A @@ ADTValue, L]] =
      _.split("\n").flatMap(_.split(s"ADTValue := ", 2).lift(1).filter(_ == label)).headOption.map(_ => value)

    implicit val readStr: Read[String] = """^"(.*)"$""".r.findFirstIn(_)
    implicit val readInt: Read[Int] = _.parseInt.toOption
    implicit val readBool: Read[Boolean] = _.parseBoolean.toOption

    implicit val readApply: Apply[Read] = new Apply[Read] {
      def map[A, B](fa: Read[A])(f: A => B): Read[B] = s => fa.read(s).map(f)
      def ap[A, B](fa: => Read[A])(f: => Read[A => B]): Read[B] = s => (f.read(s) |@| fa.read(s))(_(_))
    }

    implicit val readAlt: Alt[Read] = new Alt[Read] {
      def point[A](a: => A): Read[A] = _ => Some(a)
      def ap[A, B](fa: => Read[A])(f: => Read[A => B]): Read[B] = s => (f.read(s) |@| fa.read(s))(_(_))
      def alt[A](a1: => Read[A], a2: => Read[A]): Read[A] = s => a1.read(s).orElse(a2.read(s))
    }
  }

  trait Csv[A] { def toCsv(a: A): List[String] }
  object Csv {
    implicit val csvStr: Csv[String] = List(_)
    implicit val csvInt: Csv[Int] = i => List(i.toString)
    implicit val csvBool: Csv[Boolean] = b => List(b.toString)
    implicit def csvList[A](implicit c: Csv[A]): Csv[List[A]] = _.flatMap(c.toCsv(_))

    implicit def csvLabelled[A: Csv, L <: Singleton with String](implicit c: Csv[A]): Csv[Labelled.Aux[A, L]] =
      a => c.toCsv(a.value)

    implicit def csvAdtVal[A, L <: Singleton with String]: Csv[Labelled.Aux[A @@ ADTValue, L]] =
      a => List(a.label)

    type CsvF[A] = A => List[String]
    implicit val csvIso: IsoFunctor[Csv, CsvF] =
      IsoFunctor[Csv, CsvF](Lambda[Csv ~> CsvF](_.toCsv _), new (CsvF ~> Csv) { def apply[A](f: CsvF[A]): Csv[A] = f(_) })

    implicit val csvDecide: Decidable[Csv] = Decidable.fromIso[Csv, CsvF](csvIso)
    implicit val csvDivide: Divide[Csv] = Divide.fromIso[Csv, CsvF](csvIso)
  }
}

object types {
  import typeclasses._

  // @annotations.deriving(
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   labelledContravariant = Vector(Csv, Show, EncodeJson, Encoder)
  // )
  // sealed trait Foo
  // case object Bar extends Foo
  // @annotations.deriving(
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Baz(s: String) extends Foo

  @annotations.deriving case class NoInstances(s: String)

  @annotations.deriving(
    covariant = Vector(),
    labelledCovariant = Vector(Read, DecodeJson, Decoder),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson, Encoder)
  )
  case class Test1(
    x1: String
  )
  object Test1 {}

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Test2(
  //   x1: String,
  //   x2: Int
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Test3(
  //   x1: String,
  //   x2: Int,
  //   x3: Boolean
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Test4(
  //   x1: String,
  //   x2: Int,
  //   x3: Boolean,
  //   x4: String
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Test5(
  //   x1: String,
  //   x2: Int,
  //   x3: Boolean,
  //   x4: String,
  //   x5: Int
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Test6(
  //   x1: String,
  //   x2: Int,
  //   x3: Boolean,
  //   x4: String,
  //   x5: Int,
  //   x6: Boolean
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Test7(
  //   x1: String,
  //   x2: Int,
  //   x3: Boolean,
  //   x4: String,
  //   x5: Int,
  //   x6: Boolean,
  //   x7: String
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Test8(
  //   x1: String,
  //   x2: Int,
  //   x3: Boolean,
  //   x4: String,
  //   x5: Int,
  //   x6: Boolean,
  //   x7: String,
  //   x8: Int
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Test9(
  //   x1: String,
  //   x2: Int,
  //   x3: Boolean,
  //   x4: String,
  //   x5: Int,
  //   x6: Boolean,
  //   x7: String,
  //   x8: Int,
  //   x9: Boolean
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Test10(
  //   x1: String,
  //   x2: Int,
  //   x3: Boolean,
  //   x4: String,
  //   x5: Int,
  //   x6: Boolean,
  //   x7: String,
  //   x8: Int,
  //   x9: Boolean,
  //   x10: String
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Test11(
  //   x1: String,
  //   x2: Int,
  //   x3: Boolean,
  //   x4: String,
  //   x5: Int,
  //   x6: Boolean,
  //   x7: String,
  //   x8: Int,
  //   x9: Boolean,
  //   x10: String,
  //   x11: Int
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Test12(
  //   x1: String,
  //   x2: Int,
  //   x3: Boolean,
  //   x4: String,
  //   x5: Int,
  //   x6: Boolean,
  //   x7: String,
  //   x8: Int,
  //   x9: Boolean,
  //   x10: String,
  //   x11: Int,
  //   x12: Boolean
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Test13(
  //   x1: String,
  //   x2: Int,
  //   x3: Boolean,
  //   x4: String,
  //   x5: Int,
  //   x6: Boolean,
  //   x7: String,
  //   x8: Int,
  //   x9: Boolean,
  //   x10: String,
  //   x11: Int,
  //   x12: Boolean,
  //   x13: String
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Test14(
  //   x1: String,
  //   x2: Int,
  //   x3: Boolean,
  //   x4: String,
  //   x5: Int,
  //   x6: Boolean,
  //   x7: String,
  //   x8: Int,
  //   x9: Boolean,
  //   x10: String,
  //   x11: Int,
  //   x12: Boolean,
  //   x13: String,
  //   x14: Int
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Test15(
  //   x1: String,
  //   x2: Int,
  //   x3: Boolean,
  //   x4: String,
  //   x5: Int,
  //   x6: Boolean,
  //   x7: String,
  //   x8: Int,
  //   x9: Boolean,
  //   x10: String,
  //   x11: Int,
  //   x12: Boolean,
  //   x13: String,
  //   x14: Int,
  //   x15: Boolean
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Test16(
  //   x1: String,
  //   x2: Int,
  //   x3: Boolean,
  //   x4: String,
  //   x5: Int,
  //   x6: Boolean,
  //   x7: String,
  //   x8: Int,
  //   x9: Boolean,
  //   x10: String,
  //   x11: Int,
  //   x12: Boolean,
  //   x13: String,
  //   x14: Int,
  //   x15: Boolean,
  //   x16: String
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Test17(
  //   x1: String,
  //   x2: Int,
  //   x3: Boolean,
  //   x4: String,
  //   x5: Int,
  //   x6: Boolean,
  //   x7: String,
  //   x8: Int,
  //   x9: Boolean,
  //   x10: String,
  //   x11: Int,
  //   x12: Boolean,
  //   x13: String,
  //   x14: Int,
  //   x15: Boolean,
  //   x16: String,
  //   x17: Int
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Test18(
  //   x1: String,
  //   x2: Int,
  //   x3: Boolean,
  //   x4: String,
  //   x5: Int,
  //   x6: Boolean,
  //   x7: String,
  //   x8: Int,
  //   x9: Boolean,
  //   x10: String,
  //   x11: Int,
  //   x12: Boolean,
  //   x13: String,
  //   x14: Int,
  //   x15: Boolean,
  //   x16: String,
  //   x17: Int,
  //   x18: Boolean
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Test19(
  //   x1: String,
  //   x2: Int,
  //   x3: Boolean,
  //   x4: String,
  //   x5: Int,
  //   x6: Boolean,
  //   x7: String,
  //   x8: Int,
  //   x9: Boolean,
  //   x10: String,
  //   x11: Int,
  //   x12: Boolean,
  //   x13: String,
  //   x14: Int,
  //   x15: Boolean,
  //   x16: String,
  //   x17: Int,
  //   x18: Boolean,
  //   x19: String
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Test20(
  //   x1: String,
  //   x2: Int,
  //   x3: Boolean,
  //   x4: String,
  //   x5: Int,
  //   x6: Boolean,
  //   x7: String,
  //   x8: Int,
  //   x9: Boolean,
  //   x10: String,
  //   x11: Int,
  //   x12: Boolean,
  //   x13: String,
  //   x14: Int,
  //   x15: Boolean,
  //   x16: String,
  //   x17: Int,
  //   x18: Boolean,
  //   x19: String,
  //   x20: Int
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Test21(
  //   x1: String,
  //   x2: Int,
  //   x3: Boolean,
  //   x4: String,
  //   x5: Int,
  //   x6: Boolean,
  //   x7: String,
  //   x8: Int,
  //   x9: Boolean,
  //   x10: String,
  //   x11: Int,
  //   x12: Boolean,
  //   x13: String,
  //   x14: Int,
  //   x15: Boolean,
  //   x16: String,
  //   x17: Int,
  //   x18: Boolean,
  //   x19: String,
  //   x20: Int,
  //   x21: Boolean
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Test22(
  //   x1: String,
  //   x2: Int,
  //   x3: Boolean,
  //   x4: String,
  //   x5: Int,
  //   x6: Boolean,
  //   x7: String,
  //   x8: Int,
  //   x9: Boolean,
  //   x10: String,
  //   x11: Int,
  //   x12: Boolean,
  //   x13: String,
  //   x14: Int,
  //   x15: Boolean,
  //   x16: String,
  //   x17: Int,
  //   x18: Boolean,
  //   x19: String,
  //   x20: Int,
  //   x21: Boolean,
  //   x22: String
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class Multi(str: String)(val int: Int)


  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams1[A1](
  //   x1: A1
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams2[A1, A2](
  //   x1: A1,
  //   x2: A2
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams3[A1, A2, A3](
  //   x1: A1,
  //   x2: A2,
  //   x3: A3
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams4[A1, A2, A3, A4](
  //   x1: A1,
  //   x2: A2,
  //   x3: A3,
  //   x4: A4
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams5[A1, A2, A3, A4, A5](
  //   x1: A1,
  //   x2: A2,
  //   x3: A3,
  //   x4: A4,
  //   x5: A5
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams6[A1, A2, A3, A4, A5, A6](
  //   x1: A1,
  //   x2: A2,
  //   x3: A3,
  //   x4: A4,
  //   x5: A5,
  //   x6: A6
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams7[A1, A2, A3, A4, A5, A6, A7](
  //   x1: A1,
  //   x2: A2,
  //   x3: A3,
  //   x4: A4,
  //   x5: A5,
  //   x6: A6,
  //   x7: A7
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams8[A1, A2, A3, A4, A5, A6, A7, A8](
  //   x1: A1,
  //   x2: A2,
  //   x3: A3,
  //   x4: A4,
  //   x5: A5,
  //   x6: A6,
  //   x7: A7,
  //   x8: A8
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams9[A1, A2, A3, A4, A5, A6, A7, A8, A9](
  //   x1: A1,
  //   x2: A2,
  //   x3: A3,
  //   x4: A4,
  //   x5: A5,
  //   x6: A6,
  //   x7: A7,
  //   x8: A8,
  //   x9: A9
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
  //   x1: A1,
  //   x2: A2,
  //   x3: A3,
  //   x4: A4,
  //   x5: A5,
  //   x6: A6,
  //   x7: A7,
  //   x8: A8,
  //   x9: A9,
  //   x10: A10
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
  //   x1: A1,
  //   x2: A2,
  //   x3: A3,
  //   x4: A4,
  //   x5: A5,
  //   x6: A6,
  //   x7: A7,
  //   x8: A8,
  //   x9: A9,
  //   x10: A10,
  //   x11: A11
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
  //   x1: A1,
  //   x2: A2,
  //   x3: A3,
  //   x4: A4,
  //   x5: A5,
  //   x6: A6,
  //   x7: A7,
  //   x8: A8,
  //   x9: A9,
  //   x10: A10,
  //   x11: A11,
  //   x12: A12
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
  //   x1: A1,
  //   x2: A2,
  //   x3: A3,
  //   x4: A4,
  //   x5: A5,
  //   x6: A6,
  //   x7: A7,
  //   x8: A8,
  //   x9: A9,
  //   x10: A10,
  //   x11: A11,
  //   x12: A12,
  //   x13: A13
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
  //   x1: A1,
  //   x2: A2,
  //   x3: A3,
  //   x4: A4,
  //   x5: A5,
  //   x6: A6,
  //   x7: A7,
  //   x8: A8,
  //   x9: A9,
  //   x10: A10,
  //   x11: A11,
  //   x12: A12,
  //   x13: A13,
  //   x14: A14
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
  //   x1: A1,
  //   x2: A2,
  //   x3: A3,
  //   x4: A4,
  //   x5: A5,
  //   x6: A6,
  //   x7: A7,
  //   x8: A8,
  //   x9: A9,
  //   x10: A10,
  //   x11: A11,
  //   x12: A12,
  //   x13: A13,
  //   x14: A14,
  //   x15: A15
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
  //   x1: A1,
  //   x2: A2,
  //   x3: A3,
  //   x4: A4,
  //   x5: A5,
  //   x6: A6,
  //   x7: A7,
  //   x8: A8,
  //   x9: A9,
  //   x10: A10,
  //   x11: A11,
  //   x12: A12,
  //   x13: A13,
  //   x14: A14,
  //   x15: A15,
  //   x16: A16
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
  //   x1: A1,
  //   x2: A2,
  //   x3: A3,
  //   x4: A4,
  //   x5: A5,
  //   x6: A6,
  //   x7: A7,
  //   x8: A8,
  //   x9: A9,
  //   x10: A10,
  //   x11: A11,
  //   x12: A12,
  //   x13: A13,
  //   x14: A14,
  //   x15: A15,
  //   x16: A16,
  //   x17: A17
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
  //   x1: A1,
  //   x2: A2,
  //   x3: A3,
  //   x4: A4,
  //   x5: A5,
  //   x6: A6,
  //   x7: A7,
  //   x8: A8,
  //   x9: A9,
  //   x10: A10,
  //   x11: A11,
  //   x12: A12,
  //   x13: A13,
  //   x14: A14,
  //   x15: A15,
  //   x16: A16,
  //   x17: A17,
  //   x18: A18
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
  //   x1: A1,
  //   x2: A2,
  //   x3: A3,
  //   x4: A4,
  //   x5: A5,
  //   x6: A6,
  //   x7: A7,
  //   x8: A8,
  //   x9: A9,
  //   x10: A10,
  //   x11: A11,
  //   x12: A12,
  //   x13: A13,
  //   x14: A14,
  //   x15: A15,
  //   x16: A16,
  //   x17: A17,
  //   x18: A18,
  //   x19: A19
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
  //   x1: A1,
  //   x2: A2,
  //   x3: A3,
  //   x4: A4,
  //   x5: A5,
  //   x6: A6,
  //   x7: A7,
  //   x8: A8,
  //   x9: A9,
  //   x10: A10,
  //   x11: A11,
  //   x12: A12,
  //   x13: A13,
  //   x14: A14,
  //   x15: A15,
  //   x16: A16,
  //   x17: A17,
  //   x18: A18,
  //   x19: A19,
  //   x20: A20
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
  //   x1: A1,
  //   x2: A2,
  //   x3: A3,
  //   x4: A4,
  //   x5: A5,
  //   x6: A6,
  //   x7: A7,
  //   x8: A8,
  //   x9: A9,
  //   x10: A10,
  //   x11: A11,
  //   x12: A12,
  //   x13: A13,
  //   x14: A14,
  //   x15: A15,
  //   x16: A16,
  //   x17: A17,
  //   x18: A18,
  //   x19: A19,
  //   x20: A20,
  //   x21: A21
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class TParams22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
  //   x1: A1,
  //   x2: A2,
  //   x3: A3,
  //   x4: A4,
  //   x5: A5,
  //   x6: A6,
  //   x7: A7,
  //   x8: A8,
  //   x9: A9,
  //   x10: A10,
  //   x11: A11,
  //   x12: A12,
  //   x13: A13,
  //   x14: A14,
  //   x15: A15,
  //   x16: A16,
  //   x17: A17,
  //   x18: A18,
  //   x19: A19,
  //   x20: A20,
  //   x21: A21,
  //   x22: A22
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK1[F[_], A1](
  //   run: F[A1]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK2[F[_, _], A1, A2](
  //   run: F[A1, A2]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK3[F[_, _, _], A1, A2, A3](
  //   run: F[A1, A2, A3]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK4[F[_, _, _, _], A1, A2, A3, A4](
  //   run: F[A1, A2, A3, A4]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK5[F[_, _, _, _, _], A1, A2, A3, A4, A5](
  //   run: F[A1, A2, A3, A4, A5]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK6[F[_, _, _, _, _, _], A1, A2, A3, A4, A5, A6](
  //   run: F[A1, A2, A3, A4, A5, A6]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK7[F[_, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7](
  //   run: F[A1, A2, A3, A4, A5, A6, A7]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK8[F[_, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8](
  //   run: F[A1, A2, A3, A4, A5, A6, A7, A8]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK9[F[_, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9](
  //   run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK10[F[_, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
  //   run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK11[F[_, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
  //   run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK12[F[_, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
  //   run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK13[F[_, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
  //   run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK14[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
  //   run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK15[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
  //   run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK16[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
  //   run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK17[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
  //   run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK18[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
  //   run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK19[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
  //   run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK20[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
  //   run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK21[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
  //   run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HK22[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
  //   run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
  // )

  // @annotations.deriving(
  //   covariant = Vector(),
  //   labelledCovariant = Vector(Read, DecodeJson, Decoder),
  //   contravariant = Vector(Csv),
  //   labelledContravariant = Vector(Show, EncodeJson, Encoder)
  // )
  // case class HKFG[F[_[_]], G[_]](run: F[G])
}
