package andxor

import better.files.Dsl._
import org.scalafmt.Scalafmt
import play.twirl.api.Txt
import scalaz.syntax.std.boolean._

object Generate extends App {
  val maxN = 22

  val conf = Scalafmt.parseHoconConfig(
    """align.openParenCallSite=false
       binPack.literalArgumentLists=true
       maxColumn=200
       newlines.penalizeSingleSelectMultiArgList=false
       verticalMultiline.atDefnSite=false
       verticalMultiline.newlineAfterOpenParen=false""").get

  def mkTpeList(start: Int, end: Int): List[List[String]] = start.to(end).toList.map(1.to(_).toList.map(x => s"A${x}"))

  val tpeLists = mkTpeList(1, maxN)
  val tupleTpes = mkTpeList(9, maxN)

  def noWs(s: String): String = s.filterNot(_.isWhitespace)

  def maybeWrite(name: String, txt: Txt, format: Boolean = true): Unit = {
    val f = cwd/"core"/"src"/"main"/"scala"/"andxor"/name
    val code = "package andxor\n\n" ++ txt.toString
    if (f.notExists || noWs(f.contentAsString) != noWs(code)) {
      println(s"Writing $f")
      f.overwrite(format.fold(Scalafmt.format(code, conf).get.toString, code))
      ()
    } else {
      println(s"Skipping $f -- content is unchanged")
    }
  }

  maybeWrite("AndXor.scala", template.txt.AndXor(tpeLists))
  maybeWrite("Types.scala", template.txt.Types(tpeLists))
  tpeLists.foreach(tpes => maybeWrite(s"AndXor${tpes.length}.scala", template.txt.AndXorN(tpes)))
  maybeWrite("Combine.scala", template.txt.Combine(tpeLists.drop(2)))
  maybeWrite("MapN.scala", template.txt.MapN(tpeLists.last))
  maybeWrite("Tuple.scala", template.txt.Tuple(tupleTpes))
}
