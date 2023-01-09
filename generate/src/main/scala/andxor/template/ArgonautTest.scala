package andxor.template

object ArgonautTest {
  def apply(tpeLists: List[(List[String], Int)]) = {
    val ccTpe = (i: Int) => List("Int", "String", "Boolean").apply(i % 3)
    s"""
import andxor.scalacheck.*
import _root_.argonaut.*
import _root_.argonaut.Argonaut.*
import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.{forAllNoShrink, propBoolean}

object CirceTest extends Properties("argonaut") {
${tpeLists.map { case (tpes, i) => s"""
  case class Test$i(${tpes.zip(LazyList.from(1)).map { case (_, i) => s"x$i: ${ccTpe(i - 1)}" }.mkString(", ")})
  derives Arbitrary

  property("Test$i") = forAllNoShrink { (t: Test$i) =>
    val iso = AndXorProdIso[Test$i]
    val axoCodec = CodecJson.derived(iso.derivingLabelled[EncodeJson].divide, iso.derivingLabelled[DecodeJson].apply)
    val argCodec = casecodec$i(Test$i.apply, Test$i.unapply.andThen(t => Some((${
      range(1, i).map(j => s"t.x$j").mkString(", ")
    }))))(${
      range(1, i).map(j => s""""x$j"""").mkString(", ")
    })

    val axoEncoded = axoCodec.encode(t)
    val argEncoded = argCodec.encode(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val argDecoded = argCodec.decodeJson(axoEncoded)

    ((axoEncoded == argEncoded) :| s"encoded json was different") &&
      ((axoDecoded == argDecoded) :| "decoded json was different")
  }
"""}.mkString("\n")}
}
"""
  }
}
