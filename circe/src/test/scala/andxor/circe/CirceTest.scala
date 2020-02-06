package andxor
package circe

import andxor.scalacheck._
import io.circe.{Codec, Decoder, Encoder}
import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.{BooleanOperators, forAllNoShrink}

object CirceTest extends Properties("circe") {

  @deriving(Arbitrary, Decoder, Encoder)
  case class Test1(x0: Int)

  property(" Test1 ") = forAllNoShrink { (t: Test1) =>
    val axoCodec = Codec.from(Decoder[Test1], Encoder[Test1])
    val circeCodec = Codec.forProduct1("x0")(Test1.apply)((Test1.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }

  @deriving(Arbitrary, Decoder, Encoder)
  case class Test2(x0: Int, x1: String)

  property(" Test2 ") = forAllNoShrink { (t: Test2) =>
    val axoCodec = Codec.from(Decoder[Test2], Encoder[Test2])
    val circeCodec = Codec.forProduct2("x0", "x1")(Test2.apply)((Test2.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }

  @deriving(Arbitrary, Decoder, Encoder)
  case class Test3(x0: Int, x1: String, x2: Boolean)

  property(" Test3 ") = forAllNoShrink { (t: Test3) =>
    val axoCodec = Codec.from(Decoder[Test3], Encoder[Test3])
    val circeCodec = Codec.forProduct3("x0", "x1", "x2")(Test3.apply)((Test3.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }

  @deriving(Arbitrary, Decoder, Encoder)
  case class Test4(x0: Int, x1: String, x2: Boolean, x3: Int)

  property(" Test4 ") = forAllNoShrink { (t: Test4) =>
    val axoCodec = Codec.from(Decoder[Test4], Encoder[Test4])
    val circeCodec = Codec.forProduct4("x0", "x1", "x2", "x3")(Test4.apply)((Test4.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }

  @deriving(Arbitrary, Decoder, Encoder)
  case class Test5(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String)

  property(" Test5 ") = forAllNoShrink { (t: Test5) =>
    val axoCodec = Codec.from(Decoder[Test5], Encoder[Test5])
    val circeCodec = Codec.forProduct5("x0", "x1", "x2", "x3", "x4")(Test5.apply)((Test5.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }

  @deriving(Arbitrary, Decoder, Encoder)
  case class Test6(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean)

  property(" Test6 ") = forAllNoShrink { (t: Test6) =>
    val axoCodec = Codec.from(Decoder[Test6], Encoder[Test6])
    val circeCodec = Codec.forProduct6("x0", "x1", "x2", "x3", "x4", "x5")(Test6.apply)((Test6.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }

}
