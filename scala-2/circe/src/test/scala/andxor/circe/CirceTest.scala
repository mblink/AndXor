package andxor
package circe

import andxor.scalacheck._
import io.circe.{Codec, Decoder, Encoder}
import org.scalacheck.{Arbitrary, Prop, Properties}
import org.scalacheck.Prop.{forAllNoShrink, propBoolean}

object CirceTest extends Properties("circe") {
  private def registerProp(name: String)(prop: => Prop): Unit =
    property.update(name, prop): Unit

  @derives(Arbitrary, Decoder, Encoder)
  case class Test1(x0: Int)

  registerProp(" Test1 ")(forAllNoShrink { (t: Test1) =>
    val axoCodec = Codec.from(Decoder[Test1], Encoder[Test1])
    val circeCodec = Codec.forProduct1("x0")(Test1.apply)((Test1.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, Decoder, Encoder)
  case class Test2(x0: Int, x1: String)

  registerProp(" Test2 ")(forAllNoShrink { (t: Test2) =>
    val axoCodec = Codec.from(Decoder[Test2], Encoder[Test2])
    val circeCodec = Codec.forProduct2("x0", "x1")(Test2.apply)((Test2.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, Decoder, Encoder)
  case class Test3(x0: Int, x1: String, x2: Boolean)

  registerProp(" Test3 ")(forAllNoShrink { (t: Test3) =>
    val axoCodec = Codec.from(Decoder[Test3], Encoder[Test3])
    val circeCodec = Codec.forProduct3("x0", "x1", "x2")(Test3.apply)((Test3.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, Decoder, Encoder)
  case class Test4(x0: Int, x1: String, x2: Boolean, x3: Int)

  registerProp(" Test4 ")(forAllNoShrink { (t: Test4) =>
    val axoCodec = Codec.from(Decoder[Test4], Encoder[Test4])
    val circeCodec = Codec.forProduct4("x0", "x1", "x2", "x3")(Test4.apply)((Test4.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, Decoder, Encoder)
  case class Test5(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String)

  registerProp(" Test5 ")(forAllNoShrink { (t: Test5) =>
    val axoCodec = Codec.from(Decoder[Test5], Encoder[Test5])
    val circeCodec = Codec.forProduct5("x0", "x1", "x2", "x3", "x4")(Test5.apply)((Test5.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, Decoder, Encoder)
  case class Test6(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean)

  registerProp(" Test6 ")(forAllNoShrink { (t: Test6) =>
    val axoCodec = Codec.from(Decoder[Test6], Encoder[Test6])
    val circeCodec = Codec.forProduct6("x0", "x1", "x2", "x3", "x4", "x5")(Test6.apply)((Test6.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, Decoder, Encoder)
  case class Test7(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int)

  registerProp(" Test7 ")(forAllNoShrink { (t: Test7) =>
    val axoCodec = Codec.from(Decoder[Test7], Encoder[Test7])
    val circeCodec = Codec.forProduct7("x0", "x1", "x2", "x3", "x4", "x5", "x6")(Test7.apply)((Test7.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, Decoder, Encoder)
  case class Test8(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String)

  registerProp(" Test8 ")(forAllNoShrink { (t: Test8) =>
    val axoCodec = Codec.from(Decoder[Test8], Encoder[Test8])
    val circeCodec = Codec.forProduct8("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7")(Test8.apply)((Test8.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, Decoder, Encoder)
  case class Test9(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean)

  registerProp(" Test9 ")(forAllNoShrink { (t: Test9) =>
    val axoCodec = Codec.from(Decoder[Test9], Encoder[Test9])
    val circeCodec = Codec.forProduct9("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8")(Test9.apply)((Test9.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, Decoder, Encoder)
  case class Test10(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int)

  registerProp(" Test10 ")(forAllNoShrink { (t: Test10) =>
    val axoCodec = Codec.from(Decoder[Test10], Encoder[Test10])
    val circeCodec = Codec.forProduct10("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")(Test10.apply)((Test10.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, Decoder, Encoder)
  case class Test11(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String)

  registerProp(" Test11 ")(forAllNoShrink { (t: Test11) =>
    val axoCodec = Codec.from(Decoder[Test11], Encoder[Test11])
    val circeCodec = Codec.forProduct11("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10")(Test11.apply)((Test11.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, Decoder, Encoder)
  case class Test12(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean)

  registerProp(" Test12 ")(forAllNoShrink { (t: Test12) =>
    val axoCodec = Codec.from(Decoder[Test12], Encoder[Test12])
    val circeCodec = Codec.forProduct12("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11")(Test12.apply)((Test12.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, Decoder, Encoder)
  case class Test13(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int)

  registerProp(" Test13 ")(forAllNoShrink { (t: Test13) =>
    val axoCodec = Codec.from(Decoder[Test13], Encoder[Test13])
    val circeCodec = Codec.forProduct13("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12")(Test13.apply)((Test13.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, Decoder, Encoder)
  case class Test14(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String)

  registerProp(" Test14 ")(forAllNoShrink { (t: Test14) =>
    val axoCodec = Codec.from(Decoder[Test14], Encoder[Test14])
    val circeCodec = Codec.forProduct14("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13")(Test14.apply)((Test14.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, Decoder, Encoder)
  case class Test15(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean)

  registerProp(" Test15 ")(forAllNoShrink { (t: Test15) =>
    val axoCodec = Codec.from(Decoder[Test15], Encoder[Test15])
    val circeCodec = Codec.forProduct15("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14")(Test15.apply)((Test15.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, Decoder, Encoder)
  case class Test16(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean, x15: Int)

  registerProp(" Test16 ")(forAllNoShrink { (t: Test16) =>
    val axoCodec = Codec.from(Decoder[Test16], Encoder[Test16])
    val circeCodec = Codec.forProduct16("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15")(Test16.apply)((Test16.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, Decoder, Encoder)
  case class Test17(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean, x15: Int, x16: String)

  registerProp(" Test17 ")(forAllNoShrink { (t: Test17) =>
    val axoCodec = Codec.from(Decoder[Test17], Encoder[Test17])
    val circeCodec = Codec.forProduct17("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16")(Test17.apply)((Test17.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, Decoder, Encoder)
  case class Test18(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean, x15: Int, x16: String, x17: Boolean)

  registerProp(" Test18 ")(forAllNoShrink { (t: Test18) =>
    val axoCodec = Codec.from(Decoder[Test18], Encoder[Test18])
    val circeCodec = Codec.forProduct18("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17")(Test18.apply)((Test18.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, Decoder, Encoder)
  case class Test19(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean, x15: Int, x16: String, x17: Boolean, x18: Int)

  registerProp(" Test19 ")(forAllNoShrink { (t: Test19) =>
    val axoCodec = Codec.from(Decoder[Test19], Encoder[Test19])
    val circeCodec = Codec.forProduct19("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18")(Test19.apply)((Test19.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, Decoder, Encoder)
  case class Test20(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean, x15: Int, x16: String, x17: Boolean, x18: Int, x19: String)

  registerProp(" Test20 ")(forAllNoShrink { (t: Test20) =>
    val axoCodec = Codec.from(Decoder[Test20], Encoder[Test20])
    val circeCodec = Codec.forProduct20("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19")(Test20.apply)((Test20.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, Decoder, Encoder)
  case class Test21(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean, x15: Int, x16: String, x17: Boolean, x18: Int, x19: String, x20: Boolean)

  registerProp(" Test21 ")(forAllNoShrink { (t: Test21) =>
    val axoCodec = Codec.from(Decoder[Test21], Encoder[Test21])
    val circeCodec = Codec.forProduct21("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20")(Test21.apply)((Test21.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

  @derives(Arbitrary, Decoder, Encoder)
  case class Test22(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean, x15: Int, x16: String, x17: Boolean, x18: Int, x19: String, x20: Boolean, x21: Int)

  registerProp(" Test22 ")(forAllNoShrink { (t: Test22) =>
    val axoCodec = Codec.from(Decoder[Test22], Encoder[Test22])
    val circeCodec = Codec.forProduct22("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20", "x21")(Test22.apply)((Test22.unapply _).andThen(_.get))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| "encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  })

}
