package andxor

import andxor.syntax._
import better.files.Dsl._
import play.twirl.api.Txt
import scalariform.formatter.ScalaFormatter
import scalariform.formatter.preferences._
import scalaz.syntax.std.boolean._

object Generate extends App {
  val maxN = 2

  val conf = FormattingPreferences()
    .setPreference(NewlineAtEndOfFile, true)
    .setPreference(SpacesAroundMultiImports, false)

  val tpeLists = mkTpeList(1, maxN)

  def noWs(s: String): String = s.filterNot(_.isWhitespace)

  def maybeWrite(name: String, txt: Txt, format: Boolean = true): Unit = {
    val f = cwd/"core"/"src"/"main"/"scala"/"andxor"/name
    println(s"Generating $f...")
    println("    generating...")
    val code = "package andxor\n\n" ++ txt.toString
    if (f.notExists || noWs(f.contentAsString) != noWs(code)) {
      val toWrite = format.fold({
        println("    formatting...")
        ScalaFormatter.format(code, conf)
      }, code)
      println("    writing...")
      f.overwrite(toWrite)
      ()
    } else {
      println(s"Skipping $f -- content is unchanged")
    }
  }

  maybeWrite("AndXor.scala", template.txt.AndXor(), false)
  maybeWrite("Derivation.scala", template.txt.Derivation(tpeLists))
  maybeWrite("Types.scala", template.txt.Types(tpeLists))
  tpeLists.foreach(tpes => maybeWrite(s"AndXor${tpes.length}.scala", template.txt.AndXorN(tpes)))
  maybeWrite("Combine.scala", template.txt.Combine(tpeLists.drop(2)))
  maybeWrite("MapN.scala", template.txt.MapN(tpeLists.last))
  maybeWrite("Tuple.scala", template.txt.Tuple(tpeLists))
}
