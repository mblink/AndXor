package andxor

import scalaz.{\/, Zipper}
import scalaz.syntax.comonad._
import scalaz.syntax.id._
import scalaz.syntax.std.boolean._

object syntax {
  val tupleLen = 22

  def parens(s: String): String = s"($s)"

  implicit class TpesOps(tpes: List[String]) {
    def copName = s"Cop${tpes.length}"
    def copTpeDef = s"$copName[F[_], $tpeParams]"
    def copTpeF(F: String) = s"$copName[$F, $tpeParams]"
    def copTpe = copTpeF("F")

    def prodName = s"Prod${tpes.length}"
    def prodTpeDef = s"$prodName[F[_], $tpeParams]"
    def prodTpeF(F: String) = s"$prodName[$F, $tpeParams]"
    def prodTpe = prodTpeF("F")

    def djBase(wrapTpe: String => String): String =
      tpes.init.foldRight(wrapTpe(tpes.last))((e, a) => s"(${wrapTpe(e)} \\/ $a)")

    def dj: String = djBase(identity _)
    def djK(F: String): String = djBase(t => s"$F[$t]")

    def tupleGroups: List[String] \/ (List[List[String]], List[String]) =
      (tpes.length <= tupleLen).fold(
        \/.left(tpes),
        tpes.grouped(tupleLen).toList.init |> (groups => \/.right((groups, tpes.drop(tupleLen * groups.length)))))

    def mkTuple: String = parens(tpes.mkString(", "))

    def tupleVals(a: String, v: String, spaces: String, sIx: Int = 0): String =
      paramList(a, sIx).zipWithIndex.map(t => s"val ${t._1} = ${v}${tupleAccess(t._2 + 1)}").mkString(s"\n$spaces")

    def prodBase(wrapTpe: String => String): String =
      tpes.map(wrapTpe).mkTuple

    def prod: String = prodBase(identity _)
    def prodK(F: String): String = prodBase(t => s"$F[$t]")

    def tpeParams: String = tpes.mkString(", ")

    def paramSig(FG: List[String], a: String): String =
      tpes.zipWithIndex.map(s => s"${a}${s._2}: ${FG.foldRight(s._1)((e, a) => s"${e}[${a}]")}").mkString(", ")

    def paramSig(F: String, a: String): String =
      tpes.zipWithIndex.map(s => s"${a}${s._2}: $F[${s._1}]").mkString(", ")

    def paramSig(a: String): String =
      tpes.zipWithIndex.map(s => s"${a}${s._2}: ${s._1}").mkString(", ")

    def paramListF(f: Int => String, sIx: Int = 0): List[String] =
      tpes.zipWithIndex.map(t => f(t._2 + sIx))

    def paramList(a: String, sIx: Int = 0): List[String] =
      paramListF(i => s"${a}${i}", sIx)

    def params(a: String, sIx: Int = 0): String =
      paramList(a, sIx).mkString(", ")

    def toZipper: Zipper[String] =
      Zipper.zipper(Stream.empty[String], tpes.head, tpes.tail.toStream)

    def zipper[A](fn: Zipper[String] => A): List[A] =
      toZipper.cobind(fn).toStream.toList

    def tupleAccess(idx: Int): String = foldLen01("")(s"._$idx")

    def foldLen0[A](eq0: => A)(gt0: => A): A = (tpes.length == 0).fold(eq0, gt0)
    def foldLen01[A](lteq1: => A)(gt1: => A): A = (tpes.length <= 1).fold(lteq1, gt1)
    def foldLen[A](eq0: => A)(eq1: => A)(gt1: => A): A = foldLen0[A](eq0)((tpes.length == 1).fold(eq1, gt1))
  }

  implicit class TpesWithIndexOps(tpes: List[(String, Int)]) {
    def prod: String = tpes.map(_._1).prod
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
