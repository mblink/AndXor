package andxor
package argonaut

import andxor.scalacheck._
import _root_.argonaut._
import _root_.argonaut.Argonaut._
import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.{BooleanOperators, forAllNoShrink}

object ArgonautTest extends Properties("argonaut") {

  @deriving(Arbitrary, DecodeJson, EncodeJson)
  case class Test1(x0: Int)

  property(" Test1 ") = forAllNoShrink { (t: Test1) =>
    val axoCodec = CodecJson.derived[Test1]
    val argCodec = casecodec1(Test1.apply, Test1.unapply)("x0")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  }

  @deriving(Arbitrary, DecodeJson, EncodeJson)
  case class Test2(x0: Int, x1: String)

  property(" Test2 ") = forAllNoShrink { (t: Test2) =>
    val axoCodec = CodecJson.derived[Test2]
    val argCodec = casecodec2(Test2.apply, Test2.unapply)("x0", "x1")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  }

  @deriving(Arbitrary, DecodeJson, EncodeJson)
  case class Test3(x0: Int, x1: String, x2: Boolean)

  property(" Test3 ") = forAllNoShrink { (t: Test3) =>
    val axoCodec = CodecJson.derived[Test3]
    val argCodec = casecodec3(Test3.apply, Test3.unapply)("x0", "x1", "x2")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  }

  @deriving(Arbitrary, DecodeJson, EncodeJson)
  case class Test4(x0: Int, x1: String, x2: Boolean, x3: Int)

  property(" Test4 ") = forAllNoShrink { (t: Test4) =>
    val axoCodec = CodecJson.derived[Test4]
    val argCodec = casecodec4(Test4.apply, Test4.unapply)("x0", "x1", "x2", "x3")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  }

  @deriving(Arbitrary, DecodeJson, EncodeJson)
  case class Test5(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String)

  property(" Test5 ") = forAllNoShrink { (t: Test5) =>
    val axoCodec = CodecJson.derived[Test5]
    val argCodec = casecodec5(Test5.apply, Test5.unapply)("x0", "x1", "x2", "x3", "x4")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  }

  @deriving(Arbitrary, DecodeJson, EncodeJson)
  case class Test6(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean)

  property(" Test6 ") = forAllNoShrink { (t: Test6) =>
    val axoCodec = CodecJson.derived[Test6]
    val argCodec = casecodec6(Test6.apply, Test6.unapply)("x0", "x1", "x2", "x3", "x4", "x5")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  }

}
