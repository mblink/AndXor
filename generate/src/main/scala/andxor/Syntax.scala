package andxor

import scalaz.syntax.comonad._
import scalaz.syntax.std.boolean._
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

    def djVal(v: String): String =
      z.lefts.foldLeft(Some(z.rights).filter(_.nonEmpty).map(_ => s"-\\/($v)").getOrElse(v))((a, _) => s"\\/-($a)")

    def djFold(v: String, fail: String => String, succ: String => String): String = {
      val init = z.rights.nonEmpty.fold((x: String) => s"$x.fold(l => ${succ("l")}, r => ${fail("r")})", fail)
      val folds = 0.to(z.lefts.length - 1).foldLeft(init) { (acc, i) =>
        val (l, r) = (s"a$i", s"a${i + 1}")
        (x: String) => s"$x.fold($l => ${fail(l)}, $r => ${acc(r)})"
      }
      folds(v)
    }
  }
}
