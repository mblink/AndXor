package andxor

import andxor.syntax._
import better.files.File
import better.files.Dsl._
import play.twirl.api.Txt
import scalariform.formatter.ScalaFormatter
import scalariform.formatter.preferences._
import scalaz.syntax.std.boolean._

object Generate extends App {
  val conf = FormattingPreferences()
    .setPreference(NewlineAtEndOfFile, true)
    .setPreference(SpacesAroundMultiImports, false)

  val tpeLists = mkTpeList(2, maxLen)

  def noWs(s: String): String = s.filterNot(_.isWhitespace)

  def maybeWrite(
    name: String,
    txt: Txt,
    pkgs: List[String] = List("andxor"),
    getProj: File => File = _ / "core",
    mainOrTest: File => File = _ / "main",
    fileDir: File => File = identity _,
    format: Boolean = true
  ): Unit = {
    val f = fileDir(mainOrTest(getProj(cwd) / "src") / "scala" / "andxor") / name
    println(s"Generating $f...")
    val code = s"${pkgs.map(p => s"package $p").mkString("\n")}\n\n$txt"
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

  maybeWrite("AndXor.scala", template.txt.AndXor(tpeLists))
  maybeWrite("Derivation.scala", template.txt.Derivation(tpeLists))
  maybeWrite("Types.scala", template.txt.Types(tpeLists))
  maybeWrite("TypesTest.scala", template.txt.TypesTest(tpeLists), mainOrTest = _ / "test")
  maybeWrite("AndXor1.scala", template.txt.AndXorN(List("A1")))
  tpeLists.foreach(tpes => maybeWrite(s"AndXor${tpes.length}.scala", template.txt.AndXorN(tpes)))
  maybeWrite("Combine.scala", template.txt.Combine(tpeLists.drop(1)))
  maybeWrite("MapN.scala", template.txt.MapN(tpeLists.last))
  maybeWrite("Tuple.scala", template.txt.Tuple(tpeLists))
  maybeWrite("Route.scala", template.txt.Route(), List("andxor", "routes"), getProj = _ / "routes", fileDir = _ / "routes")
}
