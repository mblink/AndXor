package andxor.test

import andxor.{Divide, Labelled}
import andxor.argonaut._
import argonaut.{DecodeJson, EncodeJson}
import scala.annotation.Annotation
import scalaz.{Apply, Show}
import scalaz.std.option._
import scalaz.syntax.apply._
import shapeless.{Witness => W}

final class andxor extends Annotation
final class deriveCovariant(val typeclasses: AnyRef*) extends Annotation
final class deriveContravariant(val typeclasses: AnyRef*) extends Annotation

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
}

object types {
  import typeclasses._

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
  case class Test1(
    x1: String
  )

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
  case class Test2(
    x1: String,
    x2: Int
  )

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
  case class Test3(
    x1: String,
    x2: Int,
    x3: Boolean
  )

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
  case class Test4(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String
  )

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
  case class Test5(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int
  )

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
  case class Test6(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int,
    x6: Boolean
  )

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
  case class Test7(
    x1: String,
    x2: Int,
    x3: Boolean,
    x4: String,
    x5: Int,
    x6: Boolean,
    x7: String
  )

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
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

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
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

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
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

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
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

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
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

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
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

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
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

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
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

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
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

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
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

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
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

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
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

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
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

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
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

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
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

  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
  case class Multi(str: String)(val int: Int)


  @andxor
  @deriveCovariant(Read, DecodeJson)
  @deriveContravariant(Show, EncodeJson)
  case class TParam[A](foo: A)
}
