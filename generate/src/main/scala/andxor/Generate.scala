package andxor

import better.files.File
import better.files.Dsl._
import org.scalafmt.Scalafmt
import play.twirl.api.Txt

object Generate extends App {
  val conf = Scalafmt.parseHoconConfig(
    """align.openParenCallSite=false
       binPack.literalArgumentLists=true
       maxColumn=200
       newlines.penalizeSingleSelectMultiArgList=false
       verticalMultiline.atDefnSite=false
       verticalMultiline.newlineAfterOpenParen=false""").get

  val tpeLists = 2.to(22).toList.map(1.to(_).toList.map(x => s"A${x}"))

  def noWs(s: String): String = s.filterNot(_.isWhitespace)

  def maybeWrite(f: File, txt: Txt): Unit = {
    val code = "package andxor\n\n" ++ txt.toString
    if (f.notExists || noWs(f.contentAsString) != noWs(code)) {
      println(s"Writing $f")
      f.overwrite(Scalafmt.format(code, conf).get.toString)
      ()
    } else {
      println(s"Skipping $f -- content is unchanged")
    }
  }

  maybeWrite(cwd/"core"/"src"/"main"/"scala"/"andxor"/"AndXor.scala", template.txt.AndXor(tpeLists))
  tpeLists.foreach(tpes => maybeWrite(cwd/"core"/"src"/"main"/"scala"/"andxor"/s"AndXor${tpes.length}.scala", template.txt.AndXorN(tpes)))
  maybeWrite(cwd/"core"/"src"/"main"/"scala"/"andxor"/"Combine.scala", template.txt.Combine(tpeLists.drop(1)))
  maybeWrite(cwd/"core"/"src"/"main"/"scala"/"andxor"/"MapN.scala", template.txt.MapN(tpeLists.last))
}
