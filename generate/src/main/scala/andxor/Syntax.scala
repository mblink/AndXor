package andxor

import scalaz.syntax.comonad._
import scalaz.Zipper

object syntax {

  implicit class TpesOps(tpes: List[String]) {
    def breakEvery(n: Int, tabs: Int): String =
      tpes.grouped(n).toList
        .map(_.mkString(", "))
        .mkString(s",\n${(1 to tabs).map(_ => "  ").mkString}")
    def dj: String =
      tpes.init.foldRight(tpes.last)((e, a) =>
        s"(${e} \\/ ${a})")
    def djK(F: String): String =
      tpes.init.foldRight(s"${F}[${tpes.last}]")((e, a) =>
        s"(${F}[${e}] \\/ ${a})")
    def prodK(F: String): String =
      s"(${tpes.map(s => s"${F}[${s}]").mkString(", ")})"
    def prod: String = s"${tpes.mkString(", ")}"
    def paramSig(FG: List[String], a: String): String =
      tpes.zipWithIndex.map(s => s"${a}${s._2}: ${
        FG.foldRight(s._1)((e, a) => s"${e}[${a}]")}")
        .mkString(", ")
    def paramSig(F: String, a: String): String =
      tpes.zipWithIndex.map(s => s"${a}${s._2}: ${F}[${s._1}]")
        .mkString(", ")
    def paramSig(a: String): String =
      tpes.zipWithIndex.map(s => s"${a}${s._2}: ${s._1}")
        .mkString(", ")
    def paramList(a: String, sIx: Int = 0): List[String] =
      tpes.zipWithIndex.map(s => s"${a}${s._2 + sIx}")
    def params(a: String, sIx: Int = 0): String =
      paramList(a, sIx).mkString(", ")

    def zipper[A](fn: Zipper[String] => A): List[A] =
      Zipper.zipper(Stream.empty[String], tpes.head, tpes.tail.toStream)
        .cobind(fn).toStream.toList
  }

  implicit class ZipperOps[A](z: Zipper[A]) {
    def toList: List[A] = z.toStream.toList
  }
}
