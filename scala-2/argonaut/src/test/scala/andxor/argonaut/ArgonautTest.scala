package andxor
package argonaut

import andxor.scalacheck._
import _root_.argonaut._
import _root_.argonaut.Argonaut._
import org.scalacheck.{Arbitrary, Prop, Properties}
import org.scalacheck.Prop.{forAllNoShrink, propBoolean}

object ArgonautTest extends Properties("argonaut") {
  private def registerProp(name: String)(prop: => Prop): Unit =
    property.update(name, prop): Unit

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test1(x0: Int)

  registerProp(" Test1 ")(forAllNoShrink { (t: Test1) =>
    val axoCodec = CodecJson.derived[Test1]
    val argCodec = casecodec1(Test1.apply, Test1.unapply)("x0")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test2(x0: Int, x1: String)

  registerProp(" Test2 ")(forAllNoShrink { (t: Test2) =>
    val axoCodec = CodecJson.derived[Test2]
    val argCodec = casecodec2(Test2.apply, Test2.unapply)("x0", "x1")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test3(x0: Int, x1: String, x2: Boolean)

  registerProp(" Test3 ")(forAllNoShrink { (t: Test3) =>
    val axoCodec = CodecJson.derived[Test3]
    val argCodec = casecodec3(Test3.apply, Test3.unapply)("x0", "x1", "x2")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test4(x0: Int, x1: String, x2: Boolean, x3: Int)

  registerProp(" Test4 ")(forAllNoShrink { (t: Test4) =>
    val axoCodec = CodecJson.derived[Test4]
    val argCodec = casecodec4(Test4.apply, Test4.unapply)("x0", "x1", "x2", "x3")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test5(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String)

  registerProp(" Test5 ")(forAllNoShrink { (t: Test5) =>
    val axoCodec = CodecJson.derived[Test5]
    val argCodec = casecodec5(Test5.apply, Test5.unapply)("x0", "x1", "x2", "x3", "x4")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test6(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean)

  registerProp(" Test6 ")(forAllNoShrink { (t: Test6) =>
    val axoCodec = CodecJson.derived[Test6]
    val argCodec = casecodec6(Test6.apply, Test6.unapply)("x0", "x1", "x2", "x3", "x4", "x5")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test7(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int)

  registerProp(" Test7 ")(forAllNoShrink { (t: Test7) =>
    val axoCodec = CodecJson.derived[Test7]
    val argCodec = casecodec7(Test7.apply, Test7.unapply)("x0", "x1", "x2", "x3", "x4", "x5", "x6")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test8(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String)

  registerProp(" Test8 ")(forAllNoShrink { (t: Test8) =>
    val axoCodec = CodecJson.derived[Test8]
    val argCodec = casecodec8(Test8.apply, Test8.unapply)("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test9(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean)

  registerProp(" Test9 ")(forAllNoShrink { (t: Test9) =>
    val axoCodec = CodecJson.derived[Test9]
    val argCodec = casecodec9(Test9.apply, Test9.unapply)("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test10(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int)

  registerProp(" Test10 ")(forAllNoShrink { (t: Test10) =>
    val axoCodec = CodecJson.derived[Test10]
    val argCodec = casecodec10(Test10.apply, Test10.unapply)("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test11(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String)

  registerProp(" Test11 ")(forAllNoShrink { (t: Test11) =>
    val axoCodec = CodecJson.derived[Test11]
    val argCodec = casecodec11(Test11.apply, Test11.unapply)("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test12(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean)

  registerProp(" Test12 ")(forAllNoShrink { (t: Test12) =>
    val axoCodec = CodecJson.derived[Test12]
    val argCodec = casecodec12(Test12.apply, Test12.unapply)("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test13(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int)

  registerProp(" Test13 ")(forAllNoShrink { (t: Test13) =>
    val axoCodec = CodecJson.derived[Test13]
    val argCodec = casecodec13(Test13.apply, Test13.unapply)("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test14(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String)

  registerProp(" Test14 ")(forAllNoShrink { (t: Test14) =>
    val axoCodec = CodecJson.derived[Test14]
    val argCodec = casecodec14(Test14.apply, Test14.unapply)("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test15(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean)

  registerProp(" Test15 ")(forAllNoShrink { (t: Test15) =>
    val axoCodec = CodecJson.derived[Test15]
    val argCodec = casecodec15(Test15.apply, Test15.unapply)("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test16(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean, x15: Int)

  registerProp(" Test16 ")(forAllNoShrink { (t: Test16) =>
    val axoCodec = CodecJson.derived[Test16]
    val argCodec = casecodec16(Test16.apply, Test16.unapply)("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test17(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean, x15: Int, x16: String)

  registerProp(" Test17 ")(forAllNoShrink { (t: Test17) =>
    val axoCodec = CodecJson.derived[Test17]
    val argCodec = casecodec17(Test17.apply, Test17.unapply)("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test18(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean, x15: Int, x16: String, x17: Boolean)

  registerProp(" Test18 ")(forAllNoShrink { (t: Test18) =>
    val axoCodec = CodecJson.derived[Test18]
    val argCodec = casecodec18(Test18.apply, Test18.unapply)("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test19(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean, x15: Int, x16: String, x17: Boolean, x18: Int)

  registerProp(" Test19 ")(forAllNoShrink { (t: Test19) =>
    val axoCodec = CodecJson.derived[Test19]
    val argCodec = casecodec19(Test19.apply, Test19.unapply)("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test20(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean, x15: Int, x16: String, x17: Boolean, x18: Int, x19: String)

  registerProp(" Test20 ")(forAllNoShrink { (t: Test20) =>
    val axoCodec = CodecJson.derived[Test20]
    val argCodec = casecodec20(Test20.apply, Test20.unapply)("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test21(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean, x15: Int, x16: String, x17: Boolean, x18: Int, x19: String, x20: Boolean)

  registerProp(" Test21 ")(forAllNoShrink { (t: Test21) =>
    val axoCodec = CodecJson.derived[Test21]
    val argCodec = casecodec21(Test21.apply, Test21.unapply)("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, DecodeJson, EncodeJson)
  case class Test22(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean, x15: Int, x16: String, x17: Boolean, x18: Int, x19: String, x20: Boolean, x21: Int)

  registerProp(" Test22 ")(forAllNoShrink { (t: Test22) =>
    val axoCodec = CodecJson.derived[Test22]
    val argCodec = casecodec22(Test22.apply, Test22.unapply)("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20", "x21")

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| "encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  })

}
