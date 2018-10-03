package andxor

import better.files._
import better.files.Dsl._
import org.scalafmt.Scalafmt

object Generate extends App {

  val conf = Scalafmt.parseHoconConfig(
    """align.openParenCallSite=false
       binPack.literalArgumentLists=true
       maxColumn=200
       newlines.penalizeSingleSelectMultiArgList=false
       verticalMultiline.atDefnSite=false
       verticalMultiline.newlineAfterOpenParen=false""").get
  val tpeLists = (2 to 22).toList.map(n => (1 to n).toList.map(x => s"A${x}"))
  val mn = cwd/"core"/"src"/"main"/"scala"/"andxor"/"MapN.scala"
  println("Writing MapN")
  mn.overwrite("package andxor\n" ++
    Scalafmt.format(template.txt.MapN(tpeLists.last).toString, conf).get.toString)
  tpeLists.foreach { tpes =>
    println(s"Writing AndXor${tpes.length}")
    val file = cwd/"core"/"src"/"main"/"scala"/"andxor"/s"AndXor${tpes.length}.scala"
    file.overwrite("package andxor\n" ++
      Scalafmt.format(template.txt.AndXorN(tpes).toString, conf).get.toString)
  }
  val cb = cwd/"core"/"src"/"main"/"scala"/"andxor"/"Combine.scala"
  println("Writing Combine")
  cb.overwrite("package andxor\n" ++
    Scalafmt.format(template.txt.Combine(tpeLists.drop(1)).toString, conf).get.toString)
}
