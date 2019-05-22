package andxor.test

import andxor.{Decidable, Divide, Labelled}
import andxor.argonaut._
import andxor.tags._
import argonaut.{DecodeJson, EncodeJson}
import scalaz.{\/, @@, Apply, Show}
import scalaz.std.anyVal._
import scalaz.std.option._
import scalaz.std.string._
import scalaz.syntax.apply._
import scalaz.syntax.id._
import scalaz.syntax.std.string._

object typeclasses {
  implicit def showLabelled[A: Show, L <: Singleton with String]: Show[Labelled.Aux[A, L]] =
    Show.shows(l => s"""${l.label} := ${Show[A].shows(l.value)}""" ++ "\n")

  implicit val showDivide: Divide[Show] = new Divide[Show] {
    def contramap[A, B](fa: Show[A])(f: B => A): Show[B] = Show.show(b => fa.show(f(b)))
    def divide2[A1, A2, Z](fa1: => Show[A1], fa2: => Show[A2])(f: Z => (A1, A2)): Show[Z] =
      Show.show { z =>
        val (a1, a2) = f(z)
        fa1.show(a1) ++ fa2.show(a2)
      }
  }

  trait Read[A] { def read(s: String): Option[A] }
  object Read {
    implicit def readLabelled[A, L <: Singleton with String](implicit l: L, r: Read[A]): Read[Labelled.Aux[A, L]] =
      _.split("\n").flatMap(_.split(s"$l := ", 2).lift(1).flatMap(r.read(_)).map(Labelled(_, l))).headOption

    implicit val readStr: Read[String] = """^"(.*)"$""".r.findFirstIn(_)
    implicit val readInt: Read[Int] = _.parseInt.toOption
    implicit val readBool: Read[Boolean] = _.parseBoolean.toOption

    implicit val readApply: Apply[Read] = new Apply[Read] {
      def map[A, B](fa: Read[A])(f: A => B): Read[B] = s => fa.read(s).map(f)
      def ap[A, B](fa: => Read[A])(f: => Read[A => B]): Read[B] = s => (f.read(s) |@| fa.read(s))(_(_))
    }
  }

  trait Csv[A] { def toCsv(a: A): List[String] }
  object Csv {
    implicit val csvStr: Csv[String] = List(_)
    implicit val csvInt: Csv[Int] = i => List(i.toString)
    implicit val csvBool: Csv[Boolean] = b => List(b.toString)
    implicit def csvList[A](implicit c: Csv[A]): Csv[List[A]] = _.flatMap(c.toCsv(_))
    implicit def csvAdtVal[A, L <: Singleton with String]: Csv[Labelled.Aux[A @@ ADTValue, L]] =
      a => List(a.label)

    implicit val csvDivide: Divide[Csv] = new Divide[Csv] {
      def contramap[A, B](fa: Csv[A])(f: B => A): Csv[B] = b => fa.toCsv(f(b))
      def divide2[A1, A2, Z](a1: => Csv[A1], a2: => Csv[A2])(f: Z => (A1, A2)): Csv[Z] =
        f(_) |> (t => a1.toCsv(t._1) ++ a2.toCsv(t._2))
    }

    implicit val csvDecide: Decidable[Csv] = new Decidable[Csv] {
      def contramap[A, B](fa: Csv[A])(f: B => A): Csv[B] = b => fa.toCsv(f(b))
      def choose2[Z, A1, A2](a1: => Csv[A1], a2: => Csv[A2])(f: Z => (A1 \/ A2)): Csv[Z] =
        f(_).fold(a1.toCsv(_), a2.toCsv(_))
    }
  }
}

object types {
  import typeclasses._

  // @deriving(
  //   // labelledCovariant = Vector(Read, DecodeJson),
  //   labelledContravariant = Vector(Csv),
  //   // labelledContravariant = Vector(Show, EncodeJson)
  // )
  // sealed trait Foo
  // case object Bar extends Foo
  // @deriving(contravariant = Vector(Csv))
  // case class Baz(s: String) extends Foo

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test1(
    x1: String
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test2(
    x1: String,
    x2: Int
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test3(
    x1: String,
    x2: Int,
    x3: Boolean
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test4(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test5(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test6(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int,
    x6: Boolean
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test7(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int,
    x6: Boolean,
    x7: String
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test8(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int,
    x6: Boolean,
    x7: String,
    x8: Int
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test9(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int,
    x6: Boolean,
    x7: String,
    x8: Int,
    x9: Boolean
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test10(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int,
    x6: Boolean,
    x7: String,
    x8: Int,
    x9: Boolean,
    x10: String
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test11(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int,
    x6: Boolean,
    x7: String,
    x8: Int,
    x9: Boolean,
    x10: String,
    x11: Int
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test12(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int,
    x6: Boolean,
    x7: String,
    x8: Int,
    x9: Boolean,
    x10: String,
    x11: Int,
    x12: Boolean
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test13(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int,
    x6: Boolean,
    x7: String,
    x8: Int,
    x9: Boolean,
    x10: String,
    x11: Int,
    x12: Boolean,
    x13: String
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test14(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int,
    x6: Boolean,
    x7: String,
    x8: Int,
    x9: Boolean,
    x10: String,
    x11: Int,
    x12: Boolean,
    x13: String,
    x14: Int
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test15(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int,
    x6: Boolean,
    x7: String,
    x8: Int,
    x9: Boolean,
    x10: String,
    x11: Int,
    x12: Boolean,
    x13: String,
    x14: Int,
    x15: Boolean
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test16(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int,
    x6: Boolean,
    x7: String,
    x8: Int,
    x9: Boolean,
    x10: String,
    x11: Int,
    x12: Boolean,
    x13: String,
    x14: Int,
    x15: Boolean,
    x16: String
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test17(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int,
    x6: Boolean,
    x7: String,
    x8: Int,
    x9: Boolean,
    x10: String,
    x11: Int,
    x12: Boolean,
    x13: String,
    x14: Int,
    x15: Boolean,
    x16: String,
    x17: Int
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test18(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int,
    x6: Boolean,
    x7: String,
    x8: Int,
    x9: Boolean,
    x10: String,
    x11: Int,
    x12: Boolean,
    x13: String,
    x14: Int,
    x15: Boolean,
    x16: String,
    x17: Int,
    x18: Boolean
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test19(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int,
    x6: Boolean,
    x7: String,
    x8: Int,
    x9: Boolean,
    x10: String,
    x11: Int,
    x12: Boolean,
    x13: String,
    x14: Int,
    x15: Boolean,
    x16: String,
    x17: Int,
    x18: Boolean,
    x19: String
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test20(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int,
    x6: Boolean,
    x7: String,
    x8: Int,
    x9: Boolean,
    x10: String,
    x11: Int,
    x12: Boolean,
    x13: String,
    x14: Int,
    x15: Boolean,
    x16: String,
    x17: Int,
    x18: Boolean,
    x19: String,
    x20: Int
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test21(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int,
    x6: Boolean,
    x7: String,
    x8: Int,
    x9: Boolean,
    x10: String,
    x11: Int,
    x12: Boolean,
    x13: String,
    x14: Int,
    x15: Boolean,
    x16: String,
    x17: Int,
    x18: Boolean,
    x19: String,
    x20: Int,
    x21: Boolean
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Test22(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int,
    x6: Boolean,
    x7: String,
    x8: Int,
    x9: Boolean,
    x10: String,
    x11: Int,
    x12: Boolean,
    x13: String,
    x14: Int,
    x15: Boolean,
    x16: String,
    x17: Int,
    x18: Boolean,
    x19: String,
    x20: Int,
    x21: Boolean,
    x22: String
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class Multi(str: String)(val int: Int)


  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams1[A1](
    x1: A1
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams2[A1, A2](
    x1: A1,
    x2: A2
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams3[A1, A2, A3](
    x1: A1,
    x2: A2,
    x3: A3
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams4[A1, A2, A3, A4](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams5[A1, A2, A3, A4, A5](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4,
    x5: A5
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams6[A1, A2, A3, A4, A5, A6](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4,
    x5: A5,
    x6: A6
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams7[A1, A2, A3, A4, A5, A6, A7](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4,
    x5: A5,
    x6: A6,
    x7: A7
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams8[A1, A2, A3, A4, A5, A6, A7, A8](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4,
    x5: A5,
    x6: A6,
    x7: A7,
    x8: A8
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams9[A1, A2, A3, A4, A5, A6, A7, A8, A9](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4,
    x5: A5,
    x6: A6,
    x7: A7,
    x8: A8,
    x9: A9
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4,
    x5: A5,
    x6: A6,
    x7: A7,
    x8: A8,
    x9: A9,
    x10: A10
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4,
    x5: A5,
    x6: A6,
    x7: A7,
    x8: A8,
    x9: A9,
    x10: A10,
    x11: A11
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4,
    x5: A5,
    x6: A6,
    x7: A7,
    x8: A8,
    x9: A9,
    x10: A10,
    x11: A11,
    x12: A12
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4,
    x5: A5,
    x6: A6,
    x7: A7,
    x8: A8,
    x9: A9,
    x10: A10,
    x11: A11,
    x12: A12,
    x13: A13
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4,
    x5: A5,
    x6: A6,
    x7: A7,
    x8: A8,
    x9: A9,
    x10: A10,
    x11: A11,
    x12: A12,
    x13: A13,
    x14: A14
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4,
    x5: A5,
    x6: A6,
    x7: A7,
    x8: A8,
    x9: A9,
    x10: A10,
    x11: A11,
    x12: A12,
    x13: A13,
    x14: A14,
    x15: A15
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4,
    x5: A5,
    x6: A6,
    x7: A7,
    x8: A8,
    x9: A9,
    x10: A10,
    x11: A11,
    x12: A12,
    x13: A13,
    x14: A14,
    x15: A15,
    x16: A16
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4,
    x5: A5,
    x6: A6,
    x7: A7,
    x8: A8,
    x9: A9,
    x10: A10,
    x11: A11,
    x12: A12,
    x13: A13,
    x14: A14,
    x15: A15,
    x16: A16,
    x17: A17
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4,
    x5: A5,
    x6: A6,
    x7: A7,
    x8: A8,
    x9: A9,
    x10: A10,
    x11: A11,
    x12: A12,
    x13: A13,
    x14: A14,
    x15: A15,
    x16: A16,
    x17: A17,
    x18: A18
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4,
    x5: A5,
    x6: A6,
    x7: A7,
    x8: A8,
    x9: A9,
    x10: A10,
    x11: A11,
    x12: A12,
    x13: A13,
    x14: A14,
    x15: A15,
    x16: A16,
    x17: A17,
    x18: A18,
    x19: A19
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4,
    x5: A5,
    x6: A6,
    x7: A7,
    x8: A8,
    x9: A9,
    x10: A10,
    x11: A11,
    x12: A12,
    x13: A13,
    x14: A14,
    x15: A15,
    x16: A16,
    x17: A17,
    x18: A18,
    x19: A19,
    x20: A20
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4,
    x5: A5,
    x6: A6,
    x7: A7,
    x8: A8,
    x9: A9,
    x10: A10,
    x11: A11,
    x12: A12,
    x13: A13,
    x14: A14,
    x15: A15,
    x16: A16,
    x17: A17,
    x18: A18,
    x19: A19,
    x20: A20,
    x21: A21
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class TParams22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4,
    x5: A5,
    x6: A6,
    x7: A7,
    x8: A8,
    x9: A9,
    x10: A10,
    x11: A11,
    x12: A12,
    x13: A13,
    x14: A14,
    x15: A15,
    x16: A16,
    x17: A17,
    x18: A18,
    x19: A19,
    x20: A20,
    x21: A21,
    x22: A22
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK1[F[_], A1](
    run: F[A1]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK2[F[_, _], A1, A2](
    run: F[A1, A2]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK3[F[_, _, _], A1, A2, A3](
    run: F[A1, A2, A3]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK4[F[_, _, _, _], A1, A2, A3, A4](
    run: F[A1, A2, A3, A4]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK5[F[_, _, _, _, _], A1, A2, A3, A4, A5](
    run: F[A1, A2, A3, A4, A5]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK6[F[_, _, _, _, _, _], A1, A2, A3, A4, A5, A6](
    run: F[A1, A2, A3, A4, A5, A6]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK7[F[_, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7](
    run: F[A1, A2, A3, A4, A5, A6, A7]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK8[F[_, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8](
    run: F[A1, A2, A3, A4, A5, A6, A7, A8]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK9[F[_, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9](
    run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK10[F[_, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
    run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK11[F[_, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
    run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK12[F[_, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
    run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK13[F[_, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
    run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK14[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
    run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK15[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
    run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK16[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
    run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK17[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
    run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK18[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
    run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK19[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
    run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK20[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
    run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK21[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
    run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HK22[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
    run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
  )

  @deriving(
    labelledCovariant = Vector(Read, DecodeJson),
    contravariant = Vector(Csv),
    labelledContravariant = Vector(Show, EncodeJson)
  )
  case class HKFG[F[_[_]], G[_]](run: F[G])
}
