package andxor.template

object CirceTest {
  def apply(tpeLists: List[(List[String], Int)]) = {
    val ccTpe = (i: Int) => List("Int", "String", "Boolean").apply(i % 3)
    s"""
import andxor.scalacheck.*
import io.circe.{Codec, Decoder, Encoder}
import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.{forAllNoShrink, propBoolean}

object CirceTest extends Properties("circe") {
${tpeLists.map { case (tpes, i) => s"""
  case class Test$i(${tpes.zip(LazyList.from(1)).map { case (_, i) => s"x$i: ${ccTpe(i - 1)}" }.mkString(", ")})
  derives Arbitrary, Codec.AsObject

  property("Test$i") = forAllNoShrink { (t: Test$i) =>
    val iso = AndXorProdIso[Test$i]
    val axoCodec = Codec.from(iso.derivingLabelled[Decoder].apply, iso.derivingLabelled[Encoder].divide)
    val circeCodec = Codec.forProduct$i(${
      range(1, i).map(j => s""""x$j"""").mkString(", ")
    })(Test$i.apply)(Test$i.unapply.andThen(t => (${
      range(1, i).map(j => s"t.x$j").mkString(", ")
    })))

    val axoEncoded = axoCodec(t)
    val circeEncoded = circeCodec(t)

    val axoDecoded = axoCodec.decodeJson(axoEncoded)
    val circeDecoded = circeCodec.decodeJson(axoEncoded)

    ((axoEncoded == circeEncoded) :| s"encoded json was different") &&
      ((axoDecoded == circeDecoded) :| "decoded json was different")
  }
"""}.mkString("\n")}
}
"""
  }
}
