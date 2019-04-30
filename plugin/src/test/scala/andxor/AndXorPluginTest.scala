package andxor.test

import andxor.{Divide, Labelled}
import andxor.argonaut._
import argonaut.{DecodeJson, EncodeJson}
import scala.annotation.Annotation
import scalaz.{Apply, Show}
import scalaz.std.option._
import scalaz.syntax.apply._
import scalaz.syntax.id._
import shapeless.{Witness => W}

final class deriveCovariant(val typeclasses: AnyRef*) extends Annotation
final class deriveLabelledCovariant(val typeclasses: AnyRef*) extends Annotation
final class deriveContravariant(val typeclasses: AnyRef*) extends Annotation
final class deriveLabelledContravariant(val typeclasses: AnyRef*) extends Annotation

object typeclasses {
  implicit def showLabelled[A, L <: String]: Show[Labelled.Aux[A, L]] =
    Show.shows(l => s"""${l.label.value} := "${l.value}"""" ++ "\n")

  implicit val showDivide: Divide[Show] = new Divide[Show] {
    def conquer[A]: Show[A] = Show.shows(_ => "")
    def divide2[A1, A2, Z](fa1: => Show[A1], fa2: => Show[A2])(f: Z => (A1, A2)): Show[Z] =
      Show.show { z =>
        val (a1, a2) = f(z)
        fa1.show(a1) ++ fa2.show(a2)
      }
  }

  trait Read[A] { def read(s: String): Option[A] }
  object Read {
    implicit def readLabelled[A, L <: String](implicit w: W.Aux[L]): Read[Labelled.Aux[A, L]] =
      s => s"""${w.value} := "([^"]*)"""".r.findFirstIn(s).map(x => Labelled(x.asInstanceOf[A], w))

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

    implicit val csvDivide: Divide[Csv] = new Divide[Csv] {
      def conquer[A]: Csv[A] = _ => Nil
      def divide2[A1, A2, Z](a1: => Csv[A1], a2: => Csv[A2])(f: Z => (A1, A2)): Csv[Z] =
        f(_) |> (t => a1.toCsv(t._1) ++ a2.toCsv(t._2))
    }
  }
}

object types {
  import typeclasses._

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
  case class Test1(
    x1: String
  )

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
  case class Test2(
    x1: String,
    x2: Int
  )

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
  case class Test3(
    x1: String,
    x2: Int,
    x3: Boolean
  )

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
  case class Test4(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String
  )

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
  case class Test5(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int
  )

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
  case class Test6(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int,
    x6: Boolean
  )

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
  case class Test7(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int,
    x6: Boolean,
    x7: String
  )

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
  case class Multi(str: String)(val int: Int)


  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
  case class TParams1[A1](
    x1: A1
  )

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
  case class TParams2[A1, A2](
    x1: A1,
    x2: A2
  )

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
  case class TParams3[A1, A2, A3](
    x1: A1,
    x2: A2,
    x3: A3
  )

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
  case class TParams4[A1, A2, A3, A4](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4
  )

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
  case class TParams5[A1, A2, A3, A4, A5](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4,
    x5: A5
  )

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
  case class TParams6[A1, A2, A3, A4, A5, A6](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4,
    x5: A5,
    x6: A6
  )

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
  case class TParams7[A1, A2, A3, A4, A5, A6, A7](
    x1: A1,
    x2: A2,
    x3: A3,
    x4: A4,
    x5: A5,
    x6: A6,
    x7: A7
  )

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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

  @deriveLabelledCovariant(Read, DecodeJson)
  @deriveContravariant(Csv)
  @deriveLabelledContravariant(Show, EncodeJson)
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
}
