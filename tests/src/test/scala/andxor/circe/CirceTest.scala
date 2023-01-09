package andxor
package circe


import andxor.scalacheck.*
import io.circe.{Codec, Decoder, Encoder}
import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.{forAllNoShrink, propBoolean}

object CirceTest extends Properties("circe") {

  case class Test1(x1: Int)
  derives Arbitrary, Codec.AsObject

  property("Test1") = forAllNoShrink { (t: Test1) =>
    val iso = AndXorProdIso[Test1]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct1("x1")(Test1.apply)(Test1.unapply.andThen(t => (t.x1)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }


  case class Test2(x1: Int, x2: String)
  derives Arbitrary, Codec.AsObject

  property("Test2") = forAllNoShrink { (t: Test2) =>
    val iso = AndXorProdIso[Test2]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct2("x1", "x2")(Test2.apply)(Test2.unapply.andThen(t => (t.x1, t.x2)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }


  case class Test3(x1: Int, x2: String, x3: Boolean)
  derives Arbitrary, Codec.AsObject

  property("Test3") = forAllNoShrink { (t: Test3) =>
    val iso = AndXorProdIso[Test3]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct3("x1", "x2", "x3")(Test3.apply)(Test3.unapply.andThen(t => (t.x1, t.x2, t.x3)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }


  case class Test4(x1: Int, x2: String, x3: Boolean, x4: Int)
  derives Arbitrary, Codec.AsObject

  property("Test4") = forAllNoShrink { (t: Test4) =>
    val iso = AndXorProdIso[Test4]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct4("x1", "x2", "x3", "x4")(Test4.apply)(Test4.unapply.andThen(t => (t.x1, t.x2, t.x3, t.x4)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }


  case class Test5(x1: Int, x2: String, x3: Boolean, x4: Int, x5: String)
  derives Arbitrary, Codec.AsObject

  property("Test5") = forAllNoShrink { (t: Test5) =>
    val iso = AndXorProdIso[Test5]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct5("x1", "x2", "x3", "x4", "x5")(Test5.apply)(Test5.unapply.andThen(t => (t.x1, t.x2, t.x3, t.x4, t.x5)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }


  case class Test6(x1: Int, x2: String, x3: Boolean, x4: Int, x5: String, x6: Boolean)
  derives Arbitrary, Codec.AsObject

  property("Test6") = forAllNoShrink { (t: Test6) =>
    val iso = AndXorProdIso[Test6]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct6("x1", "x2", "x3", "x4", "x5", "x6")(Test6.apply)(Test6.unapply.andThen(t => (t.x1, t.x2, t.x3, t.x4, t.x5, t.x6)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }


  case class Test7(x1: Int, x2: String, x3: Boolean, x4: Int, x5: String, x6: Boolean, x7: Int)
  derives Arbitrary, Codec.AsObject

  property("Test7") = forAllNoShrink { (t: Test7) =>
    val iso = AndXorProdIso[Test7]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct7("x1", "x2", "x3", "x4", "x5", "x6", "x7")(Test7.apply)(Test7.unapply.andThen(t => (t.x1, t.x2, t.x3, t.x4, t.x5, t.x6, t.x7)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }


  case class Test8(x1: Int, x2: String, x3: Boolean, x4: Int, x5: String, x6: Boolean, x7: Int, x8: String)
  derives Arbitrary, Codec.AsObject

  property("Test8") = forAllNoShrink { (t: Test8) =>
    val iso = AndXorProdIso[Test8]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct8("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8")(Test8.apply)(Test8.unapply.andThen(t => (t.x1, t.x2, t.x3, t.x4, t.x5, t.x6, t.x7, t.x8)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }


  case class Test9(x1: Int, x2: String, x3: Boolean, x4: Int, x5: String, x6: Boolean, x7: Int, x8: String, x9: Boolean)
  derives Arbitrary, Codec.AsObject

  property("Test9") = forAllNoShrink { (t: Test9) =>
    val iso = AndXorProdIso[Test9]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct9("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")(Test9.apply)(Test9.unapply.andThen(t => (t.x1, t.x2, t.x3, t.x4, t.x5, t.x6, t.x7, t.x8, t.x9)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }


  case class Test10(x1: Int, x2: String, x3: Boolean, x4: Int, x5: String, x6: Boolean, x7: Int, x8: String, x9: Boolean, x10: Int)
  derives Arbitrary, Codec.AsObject

  property("Test10") = forAllNoShrink { (t: Test10) =>
    val iso = AndXorProdIso[Test10]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct10("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10")(Test10.apply)(Test10.unapply.andThen(t => (t.x1, t.x2, t.x3, t.x4, t.x5, t.x6, t.x7, t.x8, t.x9, t.x10)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }


  case class Test11(x1: Int, x2: String, x3: Boolean, x4: Int, x5: String, x6: Boolean, x7: Int, x8: String, x9: Boolean, x10: Int, x11: String)
  derives Arbitrary, Codec.AsObject

  property("Test11") = forAllNoShrink { (t: Test11) =>
    val iso = AndXorProdIso[Test11]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct11("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11")(Test11.apply)(Test11.unapply.andThen(t => (t.x1, t.x2, t.x3, t.x4, t.x5, t.x6, t.x7, t.x8, t.x9, t.x10, t.x11)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }


  case class Test12(x1: Int, x2: String, x3: Boolean, x4: Int, x5: String, x6: Boolean, x7: Int, x8: String, x9: Boolean, x10: Int, x11: String, x12: Boolean)
  derives Arbitrary, Codec.AsObject

  property("Test12") = forAllNoShrink { (t: Test12) =>
    val iso = AndXorProdIso[Test12]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct12("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12")(Test12.apply)(Test12.unapply.andThen(t => (t.x1, t.x2, t.x3, t.x4, t.x5, t.x6, t.x7, t.x8, t.x9, t.x10, t.x11, t.x12)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }


  case class Test13(x1: Int, x2: String, x3: Boolean, x4: Int, x5: String, x6: Boolean, x7: Int, x8: String, x9: Boolean, x10: Int, x11: String, x12: Boolean, x13: Int)
  derives Arbitrary, Codec.AsObject

  property("Test13") = forAllNoShrink { (t: Test13) =>
    val iso = AndXorProdIso[Test13]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct13("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13")(Test13.apply)(Test13.unapply.andThen(t => (t.x1, t.x2, t.x3, t.x4, t.x5, t.x6, t.x7, t.x8, t.x9, t.x10, t.x11, t.x12, t.x13)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }


  case class Test14(x1: Int, x2: String, x3: Boolean, x4: Int, x5: String, x6: Boolean, x7: Int, x8: String, x9: Boolean, x10: Int, x11: String, x12: Boolean, x13: Int, x14: String)
  derives Arbitrary, Codec.AsObject

  property("Test14") = forAllNoShrink { (t: Test14) =>
    val iso = AndXorProdIso[Test14]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct14("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14")(Test14.apply)(Test14.unapply.andThen(t => (t.x1, t.x2, t.x3, t.x4, t.x5, t.x6, t.x7, t.x8, t.x9, t.x10, t.x11, t.x12, t.x13, t.x14)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }


  case class Test15(x1: Int, x2: String, x3: Boolean, x4: Int, x5: String, x6: Boolean, x7: Int, x8: String, x9: Boolean, x10: Int, x11: String, x12: Boolean, x13: Int, x14: String, x15: Boolean)
  derives Arbitrary, Codec.AsObject

  property("Test15") = forAllNoShrink { (t: Test15) =>
    val iso = AndXorProdIso[Test15]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct15("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15")(Test15.apply)(Test15.unapply.andThen(t => (t.x1, t.x2, t.x3, t.x4, t.x5, t.x6, t.x7, t.x8, t.x9, t.x10, t.x11, t.x12, t.x13, t.x14, t.x15)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }


  case class Test16(x1: Int, x2: String, x3: Boolean, x4: Int, x5: String, x6: Boolean, x7: Int, x8: String, x9: Boolean, x10: Int, x11: String, x12: Boolean, x13: Int, x14: String, x15: Boolean, x16: Int)
  derives Arbitrary, Codec.AsObject

  property("Test16") = forAllNoShrink { (t: Test16) =>
    val iso = AndXorProdIso[Test16]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct16("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16")(Test16.apply)(Test16.unapply.andThen(t => (t.x1, t.x2, t.x3, t.x4, t.x5, t.x6, t.x7, t.x8, t.x9, t.x10, t.x11, t.x12, t.x13, t.x14, t.x15, t.x16)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }


  case class Test17(x1: Int, x2: String, x3: Boolean, x4: Int, x5: String, x6: Boolean, x7: Int, x8: String, x9: Boolean, x10: Int, x11: String, x12: Boolean, x13: Int, x14: String, x15: Boolean, x16: Int, x17: String)
  derives Arbitrary, Codec.AsObject

  property("Test17") = forAllNoShrink { (t: Test17) =>
    val iso = AndXorProdIso[Test17]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct17("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17")(Test17.apply)(Test17.unapply.andThen(t => (t.x1, t.x2, t.x3, t.x4, t.x5, t.x6, t.x7, t.x8, t.x9, t.x10, t.x11, t.x12, t.x13, t.x14, t.x15, t.x16, t.x17)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }


  case class Test18(x1: Int, x2: String, x3: Boolean, x4: Int, x5: String, x6: Boolean, x7: Int, x8: String, x9: Boolean, x10: Int, x11: String, x12: Boolean, x13: Int, x14: String, x15: Boolean, x16: Int, x17: String, x18: Boolean)
  derives Arbitrary, Codec.AsObject

  property("Test18") = forAllNoShrink { (t: Test18) =>
    val iso = AndXorProdIso[Test18]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct18("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18")(Test18.apply)(Test18.unapply.andThen(t => (t.x1, t.x2, t.x3, t.x4, t.x5, t.x6, t.x7, t.x8, t.x9, t.x10, t.x11, t.x12, t.x13, t.x14, t.x15, t.x16, t.x17, t.x18)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }


  case class Test19(x1: Int, x2: String, x3: Boolean, x4: Int, x5: String, x6: Boolean, x7: Int, x8: String, x9: Boolean, x10: Int, x11: String, x12: Boolean, x13: Int, x14: String, x15: Boolean, x16: Int, x17: String, x18: Boolean, x19: Int)
  derives Arbitrary, Codec.AsObject

  property("Test19") = forAllNoShrink { (t: Test19) =>
    val iso = AndXorProdIso[Test19]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct19("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19")(Test19.apply)(Test19.unapply.andThen(t => (t.x1, t.x2, t.x3, t.x4, t.x5, t.x6, t.x7, t.x8, t.x9, t.x10, t.x11, t.x12, t.x13, t.x14, t.x15, t.x16, t.x17, t.x18, t.x19)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }


  case class Test20(x1: Int, x2: String, x3: Boolean, x4: Int, x5: String, x6: Boolean, x7: Int, x8: String, x9: Boolean, x10: Int, x11: String, x12: Boolean, x13: Int, x14: String, x15: Boolean, x16: Int, x17: String, x18: Boolean, x19: Int, x20: String)
  derives Arbitrary, Codec.AsObject

  property("Test20") = forAllNoShrink { (t: Test20) =>
    val iso = AndXorProdIso[Test20]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct20("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20")(Test20.apply)(Test20.unapply.andThen(t => (t.x1, t.x2, t.x3, t.x4, t.x5, t.x6, t.x7, t.x8, t.x9, t.x10, t.x11, t.x12, t.x13, t.x14, t.x15, t.x16, t.x17, t.x18, t.x19, t.x20)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }


  case class Test21(x1: Int, x2: String, x3: Boolean, x4: Int, x5: String, x6: Boolean, x7: Int, x8: String, x9: Boolean, x10: Int, x11: String, x12: Boolean, x13: Int, x14: String, x15: Boolean, x16: Int, x17: String, x18: Boolean, x19: Int, x20: String, x21: Boolean)
  derives Arbitrary, Codec.AsObject

  property("Test21") = forAllNoShrink { (t: Test21) =>
    val iso = AndXorProdIso[Test21]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct21("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20", "x21")(Test21.apply)(Test21.unapply.andThen(t => (t.x1, t.x2, t.x3, t.x4, t.x5, t.x6, t.x7, t.x8, t.x9, t.x10, t.x11, t.x12, t.x13, t.x14, t.x15, t.x16, t.x17, t.x18, t.x19, t.x20, t.x21)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }


  case class Test22(x1: Int, x2: String, x3: Boolean, x4: Int, x5: String, x6: Boolean, x7: Int, x8: String, x9: Boolean, x10: Int, x11: String, x12: Boolean, x13: Int, x14: String, x15: Boolean, x16: Int, x17: String, x18: Boolean, x19: Int, x20: String, x21: Boolean, x22: Int)
  derives Arbitrary, Codec.AsObject

  property("Test22") = forAllNoShrink { (t: Test22) =>
    val iso = AndXorProdIso[Test22]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct22("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22")(Test22.apply)(Test22.unapply.andThen(t => (t.x1, t.x2, t.x3, t.x4, t.x5, t.x6, t.x7, t.x8, t.x9, t.x10, t.x11, t.x12, t.x13, t.x14, t.x15, t.x16, t.x17, t.x18, t.x19, t.x20, t.x21, t.x22)))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }

}
