package ldr

import better.files._
import better.files.Dsl._
import org.scalafmt.Scalafmt

object Generate extends App {

  val conf = Scalafmt.parseHoconConfig(
    "binPack.literalArgumentLists=true\nmaxColumn=200\nnewlines.penalizeSingleSelectMultiArgList=false\nverticalMultiline.atDefnSite=false\nverticalMultiline.newlineAfterOpenParen=false").get
  val tpeLists = (2 to 22).toList.map(n => (1 to n).toList.map(x => s"A${x}"))
  val mn = cwd/"core"/"src"/"main"/"scala"/"ldr"/"MapN.scala"
  println("Writing MapN")
  mn.overwrite("package ldr\n" ++
    Scalafmt.format(template.txt.MapN(tpeLists.last).toString, conf).get.toString)
  tpeLists.foreach { tpes =>
    println(s"Writing LDRK${tpes.length}")
    val file = cwd/"core"/"src"/"main"/"scala"/"ldr"/s"LDRK${tpes.length}.scala"
    file.overwrite("package ldr\n" ++
      Scalafmt.format(template.txt.LDRN(tpes).toString, conf).get.toString)
  }
  val cb = cwd/"core"/"src"/"main"/"scala"/"ldr"/"Combine.scala"
  println("Writing Combine")
  cb.overwrite("package ldr\n" ++
    Scalafmt.format(template.txt.Combine(tpeLists.drop(1)).toString, conf).get.toString)
}
